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
