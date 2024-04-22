open ReM
open Dst
open Parser_plaf.Ast
open Parser_plaf.Parser
       
let rec chk_expr : expr -> texpr tea_result = function 
  | Int _n -> return IntType
  | Var id -> apply_tenv id
  | IsZero(e) ->
    chk_expr e >>= fun t ->
    if t=IntType
    then return BoolType
    else error "isZero: expected argument of type int"
  | Add(e1,e2) | Sub(e1,e2) | Mul(e1,e2)| Div(e1,e2) ->
    chk_expr e1 >>= fun t1 ->
    chk_expr e2 >>= fun t2 ->
    if (t1=IntType && t2=IntType)
    then return IntType
    else error "arith: arguments must be ints"
  | ITE(e1,e2,e3) ->
    chk_expr e1 >>= fun t1 ->
    chk_expr e2 >>= fun t2 ->
    chk_expr e3 >>= fun t3 ->
    if (t1=BoolType && t2=t3)
    then return t2
    else error "ITE: condition not boolean or types of then and else do not match"
  | Let(id,e,body) ->
    chk_expr e >>= fun t ->
    extend_tenv id t >>+
    chk_expr body
  | Proc(var,Some t1,e) ->
    extend_tenv var t1 >>+
    chk_expr e >>= fun t2 ->
    return @@ FuncType(t1,t2)
  | Proc(_var,None,_e) ->
    error "proc: type declaration missing"
  | App(e1,e2) ->
    chk_expr e1 >>=
    pair_of_funcType "app: " >>= fun (t1,t2) ->
    chk_expr e2 >>= fun t3 ->
    if t1=t3
    then return t2
    else error "app: type of argument incorrect"
  | Letrec([(_id,_param,None,_,_body)],_target) | Letrec([(_id,_param,_,None,_body)],_target) ->
    error "letrec: type declaration missing"
  | Letrec([(id,param,Some tParam,Some tRes,body)],target) ->
    extend_tenv id (FuncType(tParam,tRes)) >>+
    (extend_tenv param tParam >>+
     chk_expr body >>= fun t ->
     if t=tRes 
     then chk_expr target
     else error
         "LetRec: Type of recursive function does not match
declaration")
  | Debug(_e) ->
    string_of_tenv >>= fun str ->
    print_endline str;
    error "Debug: reached breakpoint"
  | NewRef(e) -> 
    chk_expr e >>= fun t ->
    return (RefType t)
  | DeRef(e) ->
    chk_expr e >>= fun t1 ->
    (
      match t1 with
      | RefType(t2) -> return t2
      | _ -> error "deref: expected argument of type ref"
    )
  | SetRef(e1, e2) ->
    chk_expr e1 >>= fun t1 ->
    chk_expr e2 >>= fun _ ->
    (
      match t1 with
      | RefType(_) -> return UnitType
      | _ -> error "setref: expected a reference type"
    )
  | BeginEnd([]) ->
    return UnitType
  | BeginEnd(es) ->
    let rec process = fun l -> (
      match l with
      | h::[] -> chk_expr h
      | h::t -> chk_expr h >>= fun _ -> process t
    )
    in
    (process es) >>= fun t1 ->
    return t1
  | EmptyList(t) ->
    (
      match t with
      | Some(thing) -> return (ListType thing)
      | _ -> error "list constructor expected a type"
    )
  | Cons(e1, e2) ->
    chk_expr e1 >>= fun t1 ->
    chk_expr e2 >>= fun t2 ->
    (
      match t2 with
      | ListType(t3) -> if (t1=t3) then return (t2) else (error "cons expected types that match")
      | _ -> error "cons expected second argument to be a list"
    )
  | IsEmpty(e) ->
    chk_expr e >>= fun t1 ->
    (
      match t1 with
      | TreeType(_) | ListType(_) -> return (BoolType)
      | _ -> error "isempty expected list or tree"
    )
  | Hd(e) ->
    chk_expr e >>= fun t1 ->
    (
      match t1 with
      | ListType(t2) -> return t2
      | _ -> error "hd expected a list"
    )
  | Tl(e) ->
    chk_expr e >>= fun t1 -> 
    (
      match t1 with
      | ListType(_) -> return t1
      | _ -> error "tl expected a list"
    )
  | EmptyTree(t) ->
    (
      match t with
      | Some(thing) -> return (TreeType thing)
      | _ -> error "tree expected a type"
    )
  | Node(de, le, re) ->
    chk_expr de >>= fun t1 ->
    chk_expr le >>= fun t2 ->
    chk_expr re >>= fun t3 ->
    (
      match t2 with
      | TreeType(t4) -> (
        match t3 with
        | TreeType(t5) -> if (t1=t4 && t1=t5) then return (TreeType t1) else error "mismatched types"
        | _ -> error "expected a tree"
      )
      | _ -> error "expected a tree"
    )
  | CaseT(target,emptycase,id1,id2,id3,nodecase) ->
    chk_expr target >>= fun t1 ->
    chk_expr emptycase >>= fun t2 ->
    (
      match t1 with
      | TreeType(t4) -> (
        extend_tenv id1 t4 >>+
        extend_tenv id2 (TreeType t4) >>+
        extend_tenv id3 (TreeType t4) >>+
        chk_expr nodecase >>= fun t3 ->
        if (t2=t3) then (return t2) else (error "case types don't match")
      )
      | _ -> error "expected a tree"
    )
  | _ -> failwith "chk_expr: implement"    
and
  chk_prog (AProg(_,e)) =
  chk_expr e

(* Type-check an expression *)
let chk (e:string) : texpr result =
  let c = e |> parse |> chk_prog
  in run_teac c

let chkpp (e:string) : string result =
  let c = e |> parse |> chk_prog
  in run_teac (c >>= fun t -> return @@ string_of_texpr t)



