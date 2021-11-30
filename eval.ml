open MicroCamlTypes
open Utils

exception TypeError of string
exception DeclareError of string
exception DivByZeroError 

(* Provided functions - DO NOT MODIFY *)

(* Adds mapping [x:v] to environment [env] *)
let extend env x v = (x, ref v)::env

(* Returns [v] if [x:v] is a mapping in [env]; uses the
   most recent if multiple mappings for [x] are present *)
let rec lookup env x =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value)::t -> if x = var then !value else lookup t x

(* Creates a placeholder mapping for [x] in [env]; needed
   for handling recursive definitions *)
let extend_tmp env x = (x, ref (Int 0))::env

(* Updates the (most recent) mapping in [env] for [x] to [v] *)
let rec update env x v =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value)::t -> if x = var then (value := v) else update t x v
        
(* Part 1: Evaluating expressions *)

(* Evaluates MicroCaml expression [e] in environment [env],
   returning a value, or throwing an exception on error *)
let rec eval_expr env e = 
  match e with 
    Value v1 -> v1
  | ID id -> (match env with 
               [] -> raise (DeclareError ("Unbound variable " ^ id))
              |_ -> lookup env id)
  | Not not -> (match eval_expr env not with
                  Bool bool -> if bool = false then Bool (true) else Bool (false)
                | _ -> raise (TypeError "Expected type bool"))
  | Binop (op, v1, v2) -> (match op with
                              Add -> (match eval_expr env v1 with
                                        Int i1 -> (match eval_expr env v2 with
                                                    Int i2 -> Int (i1 + i2)
                                                  | _ -> raise (TypeError "Expected type int"))
                                      | _ -> raise (TypeError "Expected type int"))
                            | Sub -> (match eval_expr env v1 with
                                        Int i1 -> (match eval_expr env v2 with
                                                    Int i2 -> Int (i1 - i2)
                                                  | _ -> raise (TypeError "Expected type int"))
                                      | _ -> raise (TypeError "Expected type int"))
                            | Mult -> (match eval_expr env v1 with
                                        Int i1 -> (match eval_expr env v2 with
                                                    Int i2 -> Int (i1 * i2)
                                                  | _ -> raise (TypeError "Expected type int"))
                                      | _ -> raise (TypeError "Expected type int"))
                            | Div -> (match eval_expr env v1 with
                                        Int i1 -> (match eval_expr env v2 with
                                                    Int i2 -> if i2 != 0 then Int (i1 / i2) else raise (DivByZeroError)
                                                  | _ -> raise (TypeError "Expected type int"))
                                      | _ -> raise (TypeError "Expected type int"))
                            | Greater -> (match eval_expr env v1 with
                                        Int i1 -> (match eval_expr env v2 with
                                                    Int i2 -> Bool (i1 > i2)
                                                  | _ -> raise (TypeError "Expected type int"))
                                      | _ -> raise (TypeError "Expected type int"))
                            | Less -> (match eval_expr env v1 with
                                        Int i1 -> (match eval_expr env v2 with
                                                    Int i2 -> Bool (i1 < i2)
                                                  | _ -> raise (TypeError "Expected type int"))
                                      | _ -> raise (TypeError "Expected type int"))
                            | GreaterEqual -> (match eval_expr env v1 with
                                                 Int i1 -> (match eval_expr env v2 with
                                                              Int i2 -> Bool (i1 >= i2)
                                                             | _ -> raise (TypeError "Expected type int"))
                                                | _ -> raise (TypeError "Expected type int"))
                            | LessEqual -> (match eval_expr env v1 with
                                              Int i1 -> (match eval_expr env v2 with
                                                          Int i2 -> Bool (i1 <= i2)
                                                        | _ -> raise (TypeError "Expected type int"))
                                            | _ -> raise (TypeError "Expected type int"))
                            | Concat -> (match eval_expr env v1 with
                                          String s1 -> (match eval_expr env v2 with
                                                          String s2 -> String (s1 ^ s2)
                                                        | _ -> raise (TypeError "Expected type String"))
                                        | _ -> raise (TypeError "Expected type String"))
                            | Equal -> (match eval_expr env v1 with
                                          Int i1 -> (match eval_expr env v2 with
                                                      Int i2 -> if i1 = i2 then Bool (true) else Bool (false)
                                                    | _ -> raise (TypeError "Expected type int"))
                                        | String s1 -> (match eval_expr env v2 with
                                                          String s2 -> if s1 = s2 then Bool (true) else Bool (false)
                                                        | _ -> raise (TypeError "Expected type String"))
                                        | Bool b1 -> (match eval_expr env v2 with
                                                        Bool b2 -> if b1 = b2 then Bool (true) else Bool (false)
                                                      | _ -> raise (TypeError "Expected type bool"))
                                        | _ -> raise (TypeError "Invalid type for equal comparison"))
                            | NotEqual -> (match eval_expr env v1 with
                                            Int i1 -> (match eval_expr env v2 with
                                                        Int i2 -> if i1 != i2 then Bool (true) else Bool (false)
                                                      | _ -> raise (TypeError "Expected type int"))
                                          | String s1 -> (match eval_expr env v2 with
                                                            String s2 -> if s1 != s2 then Bool (true) else Bool (false)
                                                          | _ -> raise (TypeError "Expected type String"))
                                          | Bool b1 -> (match eval_expr env v2 with
                                                          Bool b2 -> if b1 != b2 then Bool (true) else Bool (false)
                                                        | _ -> raise (TypeError "Expected type bool"))
                                          | _ -> raise (TypeError "Invalid type for not equal comparison"))
                            | Or -> (match eval_expr env v1 with
                                      Bool b1 -> if b1 = true then Bool (true) else 
                                                    (match eval_expr env v2 with
                                                      Bool b2 -> if b2 = true then Bool (true) else Bool (false)
                                                    | _ -> raise (TypeError "Expected type bool"))
                                    | _ -> raise (TypeError "Expected type bool"))
                            | And -> (match eval_expr env v1 with
                                        Bool b1 -> if b1 = false then Bool (false) else 
                                                      (match eval_expr env v2 with
                                                        Bool b2 -> if b2 = false then Bool (false) else Bool (true)
                                                      | _ -> raise (TypeError "Expected type bool"))
                                      | _ -> raise (TypeError "Expected type bool")))
  | If (guard, v1, v2) -> (match eval_expr env guard with
                            Bool b1 -> if b1 = true then eval_expr env v1 else eval_expr env v2
                          | _ -> raise (TypeError "Expected type bool"))
  | Let (id, bool, init, body) -> if bool = false then 
                                    let eval1 = eval_expr env init in 
                                    let env1 = extend env id eval1 in
                                    eval_expr env1 body
                                  else
                                    let temp = extend_tmp env id in
                                    let eval1 = eval_expr temp init in
                                    let _ = update temp id eval1 in 
                                    eval_expr temp body
  | Fun (id, body) -> Closure(env, id, body)
  | FunctionCall (closure, v1) -> (match eval_expr env closure with
                                    Closure (env1, id, body) -> let eval1 = eval_expr env v1 in
                                                                let env2 = extend env1 id eval1 in 
                                                                eval_expr env2 body
                                  | _ -> raise (TypeError "Not a function"))

(* Part 2: Evaluating mutop directive *)

(* Evaluates MicroCaml mutop directive [m] in environment [env],
   returning a possibly updated environment paired with
   a value option; throws an exception on error *)
let eval_mutop env m =
  match m with 
    NoOp -> (env, None)
  | Def (v1, expr) ->  let temp = extend_tmp env v1 in
                       let new_val = eval_expr temp expr in
                       let _ = update temp v1 new_val in
                       (temp, Some new_val)
  | Expr expr -> let new_val = eval_expr env expr in
                 (env, Some new_val)