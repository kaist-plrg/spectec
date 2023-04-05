type name = N of string | SupN of name * string | SubN of name * string

type expr =
  | ValueE of int
  | AddE of (expr * expr)
  | MulE of (expr * expr)
  | DivE of (expr * expr)
  | VecE of (expr * expr)
  | AppE of (name * expr list)
  | NdAppE of (name * expr list)
  | RangedAppE of (name * expr * expr)
  | WithAppE of (name * expr * string)
  | ConcatE of (expr * expr)
  | LengthE of expr
  | ArityE of expr
  | FrameE
  | BitWidthE of expr
  | PropE of (expr * string)
  | IndexAccessE of (expr * expr)
  | SliceAccessE of (expr * expr * expr)
  | ForWhichE of cond
  | RecordE of (name * expr) list
  | PageSizeE
  | AfterCallE
  | ContE of expr
  | LabelNthE of expr
  | LabelE of (expr * expr)
  | NameE of name
  (* Wasm Value Expr *)
  | ConstE of name * expr
  | RefNullE of name
  (* Yet *)
  | YetE of string

and cond =
  | NotC of cond
  | AndC of (cond * cond)
  | OrC of (cond * cond)
  | EqC of (expr * expr)
  | LtC of (expr * expr)
  | DefinedC of expr
  | PartOfC of expr list
  | TopC of string
  (* Yet *)
  | YetC of string

type instr =
  | IfI of (cond * instr list * instr list)
  | WhileI of (cond * instr list)
  | RepeatI of (expr * instr list)
  | EitherI of (instr list * instr list)
  | AssertI of string
  | PushI of expr
  | PopI of expr option
  | LetI of (name * expr)
  | TrapI
  | NopI
  | ReturnI
  | InvokeI of expr
  | EnterI of (string * expr)
  | ExecuteI of (string * expr list)
  | ReplaceI of (expr * expr)
  | JumpI of expr
  (* Yet *)
  | YetI of string

type program = Program of (string * instr list)
