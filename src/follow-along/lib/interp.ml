open Ds
open Parser_plaf.Ast
open Parser_plaf.Parser
    
let rec eval_expr : expr -> int result =
  fun e ->
  match e with
  | Int(n) ->
    return n
  | Sub(e1, e2) ->
    eval_expr e1 >>= fun a1 ->
    eval_expr e2 >>= fun a2 ->
    return (a1 - a2)
  | Div(e1, e2) ->
    eval_expr e1 >>= fun a1 ->
    eval_expr e2 >>= fun a2 ->
    if a2 == 0
    then error "Division by zero"
    else return (a1 / a2)
  | Abs(e1) ->
    eval_expr e1 >>= fun a ->
    if a < 0
    then return (-a)
    else return a
  | Min(e1, e2) ->
    eval_expr e1 >>= fun a1 ->
    eval_expr e2 >>= fun a2 ->
    if a1 <= a2
    then return a1
    else return a2
  | _ -> Error "Not yet implemented"

(** [eval_prog e] evaluates program [e] *)
let eval_prog (AProg(_,e)) =
  eval_expr e

(** [interp s] parses [s] and then evaluates it *)
let interp (e:string) : int result =
  e |> parse |> eval_prog
