open Reference_interpreter
open Al.Ast
open Al.Al_util

let embedding_error = caseV ("ERROR", [])

let module_decode (bytes_val : value) : value =
  match bytes_val with
  | ListV vs ->
    let buf = Buffer.create (Array.length !vs) in
    Array.iter (function
      | NumV (`Nat n) -> Buffer.add_char buf (Char.chr (Z.to_int n))
      | _ -> failwith "module_decode: expected list of nat bytes"
    ) !vs;
    (try Decode.decode "" (Buffer.contents buf) |> Construct.al_of_module
     with Decode.Code _ -> embedding_error)
  | _ -> failwith "module_decode: expected list value for bytes"

(* module_imports(module) : (name, name, externtype)*

   Per the Wasm core spec's embedding API (embedding.rst, module_imports),
   this repackages a decoded module's import declarations as
   [(module_name, item_name, externtype)] triples, with each externtype
   "valid under the empty context" (step 6) — i.e. fully resolved, not a bare
   index into the module's own type section. [module_val]'s raw [IMPORT]
   nodes (built by [Construct.al_of_import] straight off the decoded module)
   carry exactly that kind of unresolved externtype (e.g. a func import's
   type is [_IDX i], an index — compare [module_exports]'s [EXPORT] nodes,
   whose externtype must be resolved from an [externidx] for the same
   reason). Left unresolved, an [_IDX] handed to [func_alloc] for a host
   function ends up as that host funcinst's [TYPE] verbatim (a host function
   has no module of its own for the index to resolve against — see
   [func_alloc]'s dummy moduleinst) — a raw [_IDX] tag can never equal the
   [_DEF ...] tag [$instantiate] computes for the module's *required* import
   type via [$Module_ok], so [Externaddr_ok] can never hold and instantiation
   always fails for a host-function import. Resolved here the same way
   [module_exports] resolves its half: run [$Module_ok] (driving
   [Valid.check_module]) and take the import half of its
   [importtype* -> exporttype*] result, zipped positionally against the raw
   [IMPORT] list (both walk [module.imports] in the same order). *)
let module_imports (module_val : value) : value =
  match module_val with
  | CaseV ("MODULE", _types :: imports :: _) ->
    let import_vals =
      match imports with
      | ListV vs -> Array.to_list !vs
      | _ -> failwith "module_imports: expected list value for imports"
    in
    let importtypes =
      match Interpreter.call_func "Module_ok" [ module_val ] with
      | Some (CaseV ("->", [ ListV its; _exporttypes ])) -> Array.to_list !its
      | _ -> failwith "module_imports: Module_ok did not return import types"
    in
    (try
       List.map2
         (fun import_v externtype ->
           match import_v with
           | CaseV ("IMPORT", [ module_name; item_name; _xt ]) ->
             TupV [ module_name; item_name; externtype ]
           | _ -> failwith "module_imports: expected IMPORT case")
         import_vals importtypes
       |> listV_of_list
     with Invalid_argument _ ->
       failwith "module_imports: imports/importtypes length mismatch")
  | _ -> failwith "module_imports: expected module value"

(* module_exports(module) : (name, externtype)*

   Per the Wasm core spec's embedding API (embedding.rst, module_exports),
   this repackages a decoded module's export declarations as [(name,
   externtype)] pairs. Unlike [module_imports] — where each [IMPORT] node
   already carries its own externtype — an [EXPORT] node only carries an
   [externidx] (a reference into the module's own funcs/tables/mems/globals),
   so its externtype has to be *resolved*. That resolution is exactly what
   module validation computes as a byproduct (the embedding.rst algorithm's
   own precondition: "module is valid ... with external export types
   externtype'*"), so this runs [Relation.module_ok] (registered as
   "Module_ok", which drives the reference interpreter's real
   [Valid.check_module]) to get [(importtype*, exporttype* )], keeps the export
   half, and zips it positionally against the module's own [EXPORT] list —
   both are built from the same [module.exports] in the same order, so no
   further matching by name/index is needed. *)
let module_exports (module_val : value) : value =
  match module_val with
  | CaseV
      ( "MODULE",
        _types :: _imports :: _tags :: _globals :: _mems :: _tables
        :: _funcs :: _datas :: _elems :: _start :: exports :: _ ) ->
    let export_vals =
      match exports with
      | ListV vs -> Array.to_list !vs
      | _ -> failwith "module_exports: expected list value for exports"
    in
    let exporttypes =
      match Interpreter.call_func "Module_ok" [ module_val ] with
      | Some (CaseV ("->", [ _importtypes; ListV ets ])) -> Array.to_list !ets
      | _ -> failwith "module_exports: Module_ok did not return export types"
    in
    (try
       List.map2
         (fun export_v externtype ->
           match export_v with
           | CaseV ("EXPORT", [ name; _externidx ]) -> TupV [ name; externtype ]
           | _ -> failwith "module_exports: expected EXPORT case")
         export_vals exporttypes
       |> listV_of_list
     with Invalid_argument _ ->
       failwith "module_exports: exports/exporttypes length mismatch")
  | _ -> failwith "module_exports: expected module value"

(* instance_export(moduleinst, name) : externaddr | error

   Per the Wasm core spec's embedding API (embedding.rst, instance_export),
   looks up `name` in `moduleinst`'s own EXPORTS list (each entry an
   exportinst = {NAME name, ADDR externaddr}) and returns the matching ADDR,
   or `error` if none match. A pure structural lookup — no need to drive it
   through the AL interpreter the way module_validate/module_exports do,
   since `moduleinst` (module_instantiate's own result) already carries
   fully-resolved exportinst records directly. *)
let instance_export (moduleinst : value) (name : value) : value =
  match strv_access "EXPORTS" moduleinst with
  | ListV exports ->
    (match
       Array.find_opt (fun xi -> strv_access "NAME" xi = name) !exports
     with
     | Some xi -> strv_access "ADDR" xi
     | None -> embedding_error)
  | _ -> failwith "instance_export: expected list value for EXPORTS"

(* module_validate(module) : error?

   Per the Wasm core spec's embedding API (embedding.rst, module_validate),
   returns nothing if [module_val] is valid, else [embedding_error].
   Defers to the [$Module_ok] relation (2.4-validation.modules.spectec),
   already implemented as [Relation.module_ok]: it converts [module_val] back
   to the reference interpreter's AST and runs
   [Reference_interpreter.Valid.check_module] on it, raising
   [Exception.Invalid] when the module is not valid. The moduletype computed
   by [$Module_ok] (external import/export types) is part of that relation's
   conclusion, not of [module_validate]'s interface, so it is discarded here;
   [OptV None] stands in for the spec's "return nothing". *)
let module_validate (module_val : value) : value =
  match Interpreter.call_func "Module_ok" [ module_val ] with
  | Some _ -> OptV None
  | None -> failwith "module_validate: Module_ok returned no value"
  | exception Exception.Invalid _ -> embedding_error

(* expand(deftype) : comptype

   Not part of the Wasm Core Spec's own Embedding API (embedding.rst) — a
   wjmeta/js-api-bridge-specific convenience wrapping the [$Expand] relation
   (2.1-validation.types.spectec: [Expand: deftype ~~ comptype]), already
   implemented as [Relation.expand] (drives [Types.expand_deftype]). Exists
   because js-api/index.bs's own prose destructures a [deftype] (e.g. the
   result of [func_type], or an imported function's externtype) directly as
   if it already were its underlying [params -> results] comptype, without
   ever calling out to a [$expand] step the way the Wasm Core Spec's own
   [Expand] relation requires — the same gap as [module_imports] before it was
   fixed to resolve its externtypes via [$Module_ok] above; see
   [docs/spec_errors.md] in the wjmeta side of this bridge. *)
let expand (deftype : value) : value =
  match Interpreter.call_func "Expand" [ deftype ] with
  | Some comptype -> comptype
  | None -> failwith "expand: Expand returned no value"
  | exception Exception.Invalid _ -> embedding_error

(* match_valtype(valtype1, valtype2) : bool

   Per the Wasm core spec's embedding API (embedding.rst, "Matching"): true
   iff [valtype1] matches [valtype2] under the empty context, deferring to
   [Match.match_valtype] — the same one [relation.ml]'s [val_ok]/
   [match_globaltype]/... already call with the same empty context `[]`. *)
let match_valtype (valtype1 : value) (valtype2 : value) : value =
  let t1 = Construct.al_to_valtype valtype1 in
  let t2 = Construct.al_to_valtype valtype2 in
  boolV (Match.match_valtype [] t1 t2)

(* match_externtype(externtype1, externtype2) : bool

   The [externtype]-level sibling of [match_valtype] just above (embedding.rst,
   "Matching") — [Match.match_externtype], same empty context. *)
let match_externtype (externtype1 : value) (externtype2 : value) : value =
  let xt1 = Construct.al_to_externtype externtype1 in
  let xt2 = Construct.al_to_externtype externtype2 in
  boolV (Match.match_externtype [] xt1 xt2)

(* val_default(valtype) : val

   Per the Wasm core spec's embedding API (embedding.rst, "Values"): returns
   the default value for [valtype], or [embedding_error] if none is defined
   (a non-nullable reftype) — the spec's own stated result for that case, not
   a failure. Drives the mechanized [$default_(valtype) : val?] definition
   (4.1-execution.values.spectec) through [Interpreter.call_func], like
   [expand]/[module_validate] above, rather than the reference interpreter's
   own hand-written [Value.default_value]. The mechanized `val?` result
   compiles to `OptV`, the same convention every other SpecTec `?`-type uses
   (e.g. `mut?`/`max?` throughout construct.ml). *)
let val_default (valtype_val : value) : value =
  match Interpreter.call_func "default_" [ valtype_val ] with
  | Some (OptV (Some v)) -> v
  | Some (OptV None) -> embedding_error
  | Some _ -> failwith "val_default: default_ returned an unexpected shape"
  | None -> failwith "val_default: default_ returned no value"

(* store_init() : store

   The global [Ds.Store] is authoritative for the server, so this simply
   (re)initializes it and hands the fresh store back. *)
let store_init () : value =
  Ds.Store.init ();
  Ds.Store.get ()

(* func_alloc(store, deftype, hostfunc) : (store, funcaddr)

   Defers to the spec's own allocation function
   [$allocfunc(store, deftype, funccode, moduleinst) : (store, funcaddr)]
   (4.4-execution.modules.spectec), driven through the AL interpreter, rather
   than building the funcinst by hand. [funccode = func | hostfunc]; here the
   funccode is the [hostfunc] token minted by wjmeta-bridge, i.e.
   [CaseV ("HOSTFUNC", [TextV id])], which [$allocfunc] stores as the
   funcinst's CODE and only [host.ml] reads back on invocation.

   In interp mode the translated [allocfunc] appends the funcinst to [s.FUNCS]
   *in place* (the global [Ds.Store] field is a shared growable array) and
   returns just the [funcaddr]; so the store stays authoritative and we pair the
   returned funcaddr with the (now-updated) store ourselves to form the spec
   result [(store, funcaddr)]. A host function has no real module, so we pass a
   dummy (empty) moduleinst, matching [host.ml]'s builtin funcinsts. [funcaddr]
   is a bare nat, as [Interpreter.invoke] expects. *)
let func_alloc (store : value) (deftype : value) (hostfunc : value) : value =
  Ds.Store.set store; (* install the caller's store as the global store *)
  let moduleinst = StrV [] in (* dummy module for host functions *)
  (* In interp mode the implicit store parameter is dropped (the store is the
     global [Ds.Store]), exactly as [Interpreter.invoke] takes [funcaddr; vals]
     for the 3-param [$invoke]. So [allocfunc] is called with just
     [deftype; funccode; moduleinst]. *)
  match Interpreter.call_func "allocfunc" [ deftype; hostfunc; moduleinst ] with
  | Some funcaddr -> TupV [ Ds.Store.get (); funcaddr ]
  | None -> failwith "func_alloc: allocfunc returned no value"

(* func_type(store, funcaddr) : deftype

   Per the Wasm core spec's embedding API (embedding.rst, func_type),
   [func_type(S, a) = S.FUNCS[a].TYPE] — a pure structural lookup into the
   given [store]'s FUNCS array, same idiom as [instance_export] above (and
   [relation.ml]'s [ref_ok], which reads a funcinst's TYPE the same way off
   the global store). No AL-interpreter call and no [Ds.Store] mutation
   needed: unlike [func_alloc]/[func_invoke], this can't observe or change
   store state, so it just reads straight out of the caller-supplied [store]
   value. [funcaddr] is a bare nat, as elsewhere in this module. *)
let func_type (store : value) (funcaddr : value) : value =
  match funcaddr with
  | NumV (`Nat i) -> strv_access "TYPE" (listv_nth (strv_access "FUNCS" store) (Z.to_int i))
  | _ -> failwith "func_type: expected nat funcaddr"

(* global_type(store, globaladdr) : globaltype

   [global_type(S, a) = S.GLOBALS[a].TYPE] — same structural-lookup idiom as
   [func_type] above (embedding.rst, global_type). *)
let global_type (store : value) (globaladdr : value) : value =
  match globaladdr with
  | NumV (`Nat i) -> strv_access "TYPE" (listv_nth (strv_access "GLOBALS" store) (Z.to_int i))
  | _ -> failwith "global_type: expected nat globaladdr"

(* global_read(store, globaladdr) : val

   [global_read(S, a) = S.GLOBALS[a].VALUE] — same idiom (embedding.rst,
   global_read). *)
let global_read (store : value) (globaladdr : value) : value =
  match globaladdr with
  | NumV (`Nat i) -> strv_access "VALUE" (listv_nth (strv_access "GLOBALS" store) (Z.to_int i))
  | _ -> failwith "global_read: expected nat globaladdr"

(* global_write(store, globaladdr, val) : store | error

   Per embedding.rst: if the global's mut is empty (immutable), return
   [error]; otherwise replace GLOBALS[a].VALUE with [v] and return the
   updated store. Unlike [func_type]/[global_read], this *does* mutate: a
   globalinst's fields are [Record]'s own [ref] cells (see
   [util/record.ml]), so [Record.replace] updates GLOBALS[a].VALUE in place
   and the same (now-updated) [store] value is what's returned — no
   [Ds.Store]/AL-interpreter involvement needed, same as the rest of this
   module's pure structural embedding functions. *)
let global_write (store : value) (globaladdr : value) (v : value) : value =
  match globaladdr with
  | NumV (`Nat i) ->
    let gi = listv_nth (strv_access "GLOBALS" store) (Z.to_int i) in
    (match strv_access "TYPE" gi, gi with
     | CaseV (_, OptV None :: _), _ -> embedding_error
     | CaseV (_, OptV (Some _) :: _), StrV r ->
       Util.Record.replace "VALUE" v r; store
     | _ -> failwith "global_write: unexpected globalinst/globaltype shape")
  | _ -> failwith "global_write: expected nat globaladdr"

(* mem_type(store, memaddr) : memtype

   [mem_type(S, a) = S.MEMS[a].TYPE] — same structural-lookup idiom as
   [func_type]/[global_type] above (embedding.rst, mem_type). *)
let mem_type (store : value) (memaddr : value) : value =
  match memaddr with
  | NumV (`Nat i) -> strv_access "TYPE" (listv_nth (strv_access "MEMS" store) (Z.to_int i))
  | _ -> failwith "mem_type: expected nat memaddr"

(* mem_size(store, memaddr) : u64

   [mem_size(S, a) = |S.MEMS[a].BYTES| / 65536] (embedding.rst, mem_size) —
   the memory instance's length in Wasm pages (64 KiB each). Pure structural
   computation on the caller-supplied [store], no AL-interpreter call
   needed, same idiom as [func_type]/[global_type] above. *)
let mem_size (store : value) (memaddr : value) : value =
  match memaddr with
  | NumV (`Nat i) ->
    let bytes_ = strv_access "BYTES" (listv_nth (strv_access "MEMS" store) (Z.to_int i)) in
    natV (Z.of_int (listv_len bytes_ / 65536))
  | _ -> failwith "mem_size: expected nat memaddr"

(* mem_read_bytes(memaddr) : byte*

   Not part of the Wasm Core Spec's own Embedding API (embedding.rst's
   [mem_read] is byte-at-a-time, and — unlike this one — unused by js-api
   itself; see docs/hardcodes.md on the wjmeta side). A wjmeta-bridge-specific
   bulk read of [store.MEMS[memaddr].BYTES] wholesale, used by
   ExpandDataBlockOfPass (the "a Data Block which is identified with the
   underlying memory of |memaddr|" idiom in
   create_a_fixed_length_memory_buffer/refresh_the_memory_buffer).

   Deliberately *implicit*-store, unlike [func_type]/[global_read] above
   (mirrors [func_alloc]/[func_invoke]'s own "the global [Ds.Store] is
   authoritative" convention instead) — always reads whatever [Ds.Store]
   currently holds, no separate "caller's store" argument needed. *)
let mem_read_bytes (memaddr : value) : value =
  let store = Ds.Store.get () in
  match memaddr with
  | NumV (`Nat i) -> strv_access "BYTES" (listv_nth (strv_access "MEMS" store) (Z.to_int i))
  | _ -> failwith "mem_read_bytes: expected nat memaddr"

(* mem_grow(store, memaddr, n: u64) : store | error

   Defers to the spec's own [$growmem(meminst, nat) : meminst]
   (4.0-execution.configurations.spectec, [hint(partial)] — a genuine partial
   function: some inputs have no matching equation at all, [n] is a
   page-count delta), driven through the AL interpreter, mirroring
   [func_alloc]'s "call the mechanized AL function" pattern above rather than
   [global_write]'s pure-structural one. Unlike [$allocfunc], [$growmem]
   takes a bare [meminst], not [(store, memaddr)] — read [MEMS[i]] out of the
   caller's [store], call [growmem] on it, and on success replace [MEMS[i]]
   in place (the global array backing a [ListV] is a mutable
   [value array ref], same idiom [func_alloc]'s own doc describes for
   appending to [FUNCS]). [Interpreter.call_func] returns [None] exactly when
   no equation matched — the legitimate "grow failed" outcome for a
   [hint(partial)] function, unlike [func_alloc]/[module_validate]'s own
   [None] cases above (which [failwith], since those two are supposed to be
   total for valid inputs). *)
let mem_grow (store : value) (memaddr : value) (n : value) : value =
  match memaddr with
  | NumV (`Nat i) ->
    let mems = strv_access "MEMS" store in
    let mi = listv_nth mems (Z.to_int i) in
    (match Interpreter.call_func "growmem" [ mi; n ] with
     | Some mi' ->
       (match mems with
        | ListV arr_ref -> Array.set !arr_ref (Z.to_int i) mi'; store
        | _ -> failwith "mem_grow: unexpected MEMS shape")
     | None -> embedding_error)
  | _ -> failwith "mem_grow: expected nat memaddr"

(* func_invoke(store, funcaddr, val* ) : (store, val* | exception | error)

   The caller's [store] is installed as the global store first; [vals] is
   already a list value. [Interpreter.invoke] initializes the Wasm context
   internally and drives the AL [invoke] algorithm, which for a host-function
   funcaddr runs the [call_ref-host] rule -> [$callhostfunc] (see [host.ml]).
   The post-state is read back from [Ds.Store].

   Per embedding.rst's three-way result, a plain trap (e.g. [unreachable])
   becomes [ERROR], mirroring [module_instantiate] below. A genuine Wasm
   exception ([Exception.Throw], the exception-handling proposal's [throw])
   is kept distinct and reported as [EXCEPTION exnaddr] -- but [exnaddr] here
   is a stub ([natV_of_int 0]): [interpreter.ml]'s [ThrowI _ -> raise
   Exception.Throw] already discards the actual thrown value before it can
   reach here, and none of [exn_alloc]/[exn_tag]/[exn_read] exist on this
   embedding boundary yet, so no fixture can observe the stub. Real support
   needs threading the thrown value through [Exception.Throw] itself. *)
let func_invoke (store : value) (funcaddr : value) (vals : value) : value =
  Ds.Store.set store; (* install the caller's store as the global store *)
  match Interpreter.invoke [ funcaddr; vals ] with
  | results -> TupV [ Ds.Store.get (); results ]
  | exception Exception.Trap -> TupV [ Ds.Store.get (); embedding_error ]
  | exception Exception.Throw ->
    TupV [ Ds.Store.get (); caseV ("EXCEPTION", [ natV_of_int 0 ]) ]

(* module_instantiate(store, module, externval* ) : (store, moduleinst | error)

   Defers to the spec's own [$instantiate(module, externval* ) : moduleinst],
   driven through [Interpreter.instantiate] — which, like [Interpreter.invoke],
   initializes a fresh Wasm context internally. The caller's [store] is
   installed as the global store first, and [externvals] is already a list
   value (as with [func_invoke]'s [vals]).

   Instantiation can fail either by trapping (e.g. an out-of-bounds active
   segment) or by the module's start function throwing (Wasm
   exception-handling proposal). Both collapse to [embedding_error] here:
   [Exception.Throw] carries no payload on the OCaml side ([interpreter.ml]'s
   [ThrowI _ -> raise Exception.Throw] discards the thrown value), so there is
   no distinct "exception" result to return yet, only trap-shaped "error". *)
let module_instantiate
  (store : value) (module_ : value) (externvals : value) : value =
  Ds.Store.set store;
  match Interpreter.instantiate [ module_; externvals ] with
  | moduleinst -> TupV [ Ds.Store.get (); moduleinst ]
  | exception (Exception.Trap | Exception.Throw) ->
    TupV [ Ds.Store.get (); embedding_error ]
