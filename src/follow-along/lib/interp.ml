open Ds
open Parser_plaf.Ast
open Parser_plaf.Parser
    
let rec eval_expr : expr -> int result =
  fun e ->
  match e with
  | Int(n) -> Ok n
  | Sub(e1, e2) -> (match eval_expr e1 with
    | Error s -> Error s
    | Ok m -> (match eval_expr e2 with
      | Error s -> Error s
      | Ok n -> Ok (m-n)))
  | Div(e1, e2) -> (match eval_expr e1 with
    | Error s -> Error s
    | Ok m -> (match eval_expr e2 with
      | Error s -> Error s
      | Ok n -> if n == 0
        then Error "Division by zero"
        else Ok (m/n)))
  | _ -> Error "Not yet implemented"

(** [eval_prog e] evaluates program [e] *)
let eval_prog (AProg(_,e)) =
  eval_expr e

(** [interp s] parses [s] and then evaluates it *)
let interp (e:string) : int result =
  e |> parse |> eval_prog
