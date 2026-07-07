open Reference_interpreter
open Al.Ast
open Al.Al_util

let embedding_error = caseV ("error", [])

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
   [(module_name, item_name, externtype)] triples. [module_val] is the AL
   value produced by [module_decode], a [MODULE] case whose second field is
   already the list of [IMPORT] entries built by [Construct.al_of_import]
   ([module_name; item_name; externtype]); we just pull that field out and
   reshape each entry into a bare tuple, since [module_decode] stores the
   externtype declared by the import directly (no separate validation pass
   is needed to resolve it).

   TODO: the spec's pre-condition (module is valid, step 1) and
   post-condition (each returned externtype is valid under the empty
   context, step 6) are not checked here — [module_val] is trusted as-is. *)
let module_imports (module_val : value) : value =
  match module_val with
  | CaseV ("MODULE", _types :: imports :: _) ->
    (match imports with
     | ListV vs ->
       Array.to_list !vs
       |> List.map (function
            | CaseV ("IMPORT", [ module_name; item_name; xt ]) ->
              TupV [ module_name; item_name; xt ]
            | _ -> failwith "module_imports: expected IMPORT case")
       |> listV_of_list
     | _ -> failwith "module_imports: expected list value for imports")
  | _ -> failwith "module_imports: expected module value"

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

(* func_invoke(store, funcaddr, val* ) : (store, val* )

   The caller's [store] is installed as the global store first; [vals] is
   already a list value. [Interpreter.invoke] initializes the Wasm context
   internally and drives the AL [invoke] algorithm, which for a host-function
   funcaddr runs the [call_ref-host] rule -> [$callhostfunc] (see [host.ml]).
   The post-state is read back from [Ds.Store]. *)
let func_invoke (store : value) (funcaddr : value) (vals : value) : value =
  Ds.Store.set store; (* install the caller's store as the global store *)
  let results = Interpreter.invoke [ funcaddr; vals ] in
  TupV [ Ds.Store.get (); results ]
