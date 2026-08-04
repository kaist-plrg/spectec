exception Invalid of exn * Printexc.raw_backtrace
exception Trap
(* Carries the thrown [ref.exn]'s exnaddr. *)
exception Throw of Al.Ast.value
exception OutOfMemory
exception Timeout
exception MissingReturnValue of string
exception ArgMismatch of string
exception UnknownFunc of string
exception FreeVar of string
exception WrongConversion of string
exception Fail

(* For AL-level debugging *)
exception Error of Util.Source.region * string * string
