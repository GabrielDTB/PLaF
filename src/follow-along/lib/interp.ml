open Ds
open Parser_plaf.Ast
open Parser_plaf.Parser
    
let rec eval_expr : expr -> env -> exp_val result =
  fun e en ->
  match e with
  | Int(n) -> return (NumVal n)
  | Sub(e1, e2) ->
    eval_expr e1 en >>=
    int_of_numVal >>= fun n1 ->
    eval_expr e2 en >>=
    int_of_numVal >>= fun n2 ->
    return (NumVal (n1 - n2))
  (*
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
  *)
  | _ -> failwith "Not implemented yet!"

let eval_prog (AProg(_,e)) =
  eval_expr e

let interp (s:string) : exp_val result =
  let c = s |> parse |> eval_prog
  in c EmptyEnv
