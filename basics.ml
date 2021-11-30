(***********************************)
(* Part 1: Non-Recursive Functions *)
(***********************************)

let rev_tup tup = 
    let (x, y, z) = tup in 
        (z, y, x)
;;

let is_odd x = 
    if x mod 2 = 0 then 
        false
    else
        true
;;

let abs x = 
    if x < 0 then (-x) else x
;;

let area x y = 
    let (a, b) = x in
    let (c, d) = y in
    (abs (d - b)) * (abs (c - a))
;;

let volume x y = 
    let (a, b, c) = x in
    let (d, e, f) = y in
    (abs(f - c)) * (abs(e - b)) * (abs(d - a))
;;

(*******************************)
(* Part 2: Recursive Functions *)
(*******************************)

let rec fibonacci n = 
    if n = 0 then
        0
    else if n = 1 then
        1
    else 
        fibonacci (n - 1) + fibonacci (n - 2)
;;

let rec pow x y = 
    if y = 0 then
        1
    else
        x * (pow x (y - 1))
;;

let rec log x y = 
    if x = y then
        1
    else if x > y then
        0
    else
        1 + (log x (y / x))
;;

let rec gcf x y = 
    if y = 0 then
        x
    else
        gcf y (x mod y)
;;


let rec is_prime_helper x n =
    if x < 2 then
        false
    else if x = 2 then 
        true
    else if x mod n = 0 then
        false
    else if (n * n) > x then
        true
    else 
        is_prime_helper x (n + 1)
;;

let rec is_prime x = 
    is_prime_helper x 2
;;

(*****************)
(* Part 3: Lists *)
(*****************)

let rec get idx lst = 
    match lst with
     [] -> failwith "Out of bounds"
    | h::t -> if idx = 0 then h else get (idx - 1) t
;;

let rec length lst =
    match lst with
     [] -> 0
    | h::t -> 1 + length t
;;

let larger lst1 lst2 = 
    if (length lst1) > (length lst2) then
        lst1
    else if (length lst2) > (length lst1) then
        lst2
    else
        []
;;

let rec combine lst1 lst2 = 
    match lst1 with
     [] -> lst2
    | h::t -> h::(combine t lst2)
;;

let rec reverse lst = 
    match lst with
     [] -> []
    | [x] -> [x]
    | h::t -> combine (reverse t) [h]
;;

let rec merge lst1 lst2 = 
    match lst1, lst2 with
     [], _ -> lst2
    | _, [] -> lst1
    | h1::t1, h2::t2 -> if h1 < h2 then h1::(merge t1 lst2) else h2::(merge lst1 t2)
;;

let rec first_half shift lst =
    match lst with 
    | [] -> []
    | h::t -> if shift == 0 then lst else first_half (shift - 1) t
;;

let rec second_half shift lst =
    match lst with 
    | [] -> []
    | h::t -> if shift == 0 then [] else h::(second_half (shift - 1) t)
;;

let rotate shift lst = 
    combine (first_half (shift mod (length lst)) lst) (second_half (shift mod (length lst)) lst)

let is_palindrome lst = 
    lst = (reverse lst)