open Reference_interpreter
open Ast
open Types
open Source

(*
open Util.Source
open Il.Ast
open Xl.Atom
open Xl.Mixop
let (%) it note = it $$ no_region % note

let noinfo : info = { def=""; case="" }

let varT (typname: string) : typ = VarT (typname $ no_region, []) $ no_region
let iterT (typ: typ) (iter: iter) : typ = IterT (typ, iter) $ no_region

let tupE (exps: exp list) : exp =
  let tup_typ = TupT (List.map (fun e -> e, e.note) exps) $ no_region in

  TupE exps % tup_typ

let caseE (typname: string) (case: string) (args: exp list) : exp =
  let atoms : atom list = if case <> "" then [ Atom case % noinfo ] else [] in
  let mixop : mixop = atoms :: List.map (fun _ -> []) args in

  CaseE (mixop, tupE args) % varT typname

let listE (typname: string) (exps: exp list) : exp =
  ListE exps % iterT (varT typname) List
*)

let empty_module' =
  { types=[];
    globals=[];
    tables=[];
    memories=[];
    tags=[];
    funcs=[];
    start=None;
    elems=[];
    datas=[];
    imports=[];
    exports=[];
  }


let idx i = Int32.of_int i @@ no_region

(* Subtype test template with `subtype0` and `subtype1` *)
(*
  (module
    (type $t0 `subtype0`)
    (type $t1 `subtype1`)

    (func $f0 (param $r (ref $t0))
      (call $f0 (local.get $r))
    )
    (func $f1 (param $r (ref $t1))
      (call $f0 (local.get $r))
    )
  )
*)

let subtype (subtype0: sub_type) (subtype1: sub_type) : module_ =
  let f0 =
    { ftype=idx 0;
      locals=[];
      body=
        [ LocalGet (idx 0) @@ no_region;
          Call (idx 0) @@ no_region;
        ];
    } @@ no_region
  in

  let f1 = { f0.it with ftype=idx 1 } @@ no_region in
  { empty_module' with
    types=[ RecT [ subtype0; subtype1 ] @@ no_region ];
    funcs=[ f0; f1 ];
  } @@ no_region


(* Tagtype test template with `subtype0` and `subtype1` *)
(*
  (module
    (type $0 `subtype1`)
    (type $1 `subtype0`)
    (tag $0 (type 1))
    (export "tag" (tag 0))
  )
  (register "export")
  (module
    (type $0 `subtype1`)
    (import "export" "tag" (tag $0 (type 0)))
  )
*)


let tagtype (subtype_pair: sub_type * sub_type) : module_ list =
  let subtype0, subtype1 = subtype_pair in
  let rectype0, rectype1 = RecT [ subtype0 ], RecT [ subtype1 ] in

  let module_name = Utf8.decode "export" in
  let item_name = Utf8.decode "tag" in

  let tag_export =
    { name=item_name;
      edesc=TagExport (idx 0) @@ no_region
    } @@ no_region in

  let export_module =
    { empty_module' with
      types=[ rectype1 @@ no_region; rectype0 @@ no_region ];
      tags=[ { tgtype=idx 1 } @@ no_region ];
      exports=[ tag_export ]
    } @@ no_region in

  let tag_import =
    { module_name;
      item_name;
      idesc=TagImport (idx 0) @@ no_region;
    } @@ no_region in

  let import_module =
    { empty_module' with
      types=[ rectype1 @@ no_region ];
      imports=[ tag_import ]
    } @@ no_region in

  [ export_module; import_module ]

(*
let tmp il =

  Gen.types' "nat";

  let subtype0 = SubT (NoFinal, [], DefFuncT (FuncT ([ NumT I32T ], []))) in
  let subtype1 = SubT (NoFinal, [], DefFuncT (FuncT ([ NumT I64T ], []))) in
  let module_ = subtype subtype0 subtype1 in

  let oc = Out_channel.open_text "subtype.wat" in

  Print.module_ oc 0 module_;
  Run.run_file "subtype.wat" |> ignore;




  let tagtype0 = DefT (RecT [ subtype0 ], I32.zero) in
  let tagtype1 = DefT (RecT [ subtype1 ], I32.zero) in

  let export_module, import_module = tagtype tagtype0 tagtype1 in

  let oc = Out_channel.open_text "export.wat" in

  Print.module_ oc 0 export_module;
  Run.run_file "export.wat" |> ignore;

  let oc = Out_channel.open_text "import.wat" in

  Print.module_ oc 0 import_module;
  Run.run_file "import.wat" |> ignore
*)








