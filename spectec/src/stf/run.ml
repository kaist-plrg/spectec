open Reference_interpreter
open Ast

let list2pair (l: 'a list) : 'a * 'a =
  assert (List.length l = 2);
  List.hd l, List.hd (List.tl l)

let write (filename: string) (module_:module_) : unit =
  let oc = Out_channel.open_text filename in
  Print.module_ oc 80 module_;
  Out_channel.close oc

let stf il =

  Gen.init il;

  (* Test tagtype subtyping *)
  let target = "subtype" in
  let subtypes =
    target
    |> Gen.types 6
    |> List.map Il2al.Translate.translate_exp
    |> List.map (Backend_interpreter.Interpreter.eval_expr Backend_interpreter.Ds.Env.empty)
    |> List.map Backend_interpreter.Construct.al_to_sub_type
  in

  (* tagtype pairs *)
  [ subtypes; subtypes ]
  |> Gen.cartesian_product
  |> List.map list2pair
  (* generate test code *)
  |> List.map Template.tagtype
  |> List.iteri
    (fun i modules ->
      List.iteri (fun j module_ ->
        let filename = "out/"^target^string_of_int i ^ "-" ^ string_of_int j^".wat" in
        write filename module_;
        Run.run_file filename |> ignore
      )
    modules
    )
