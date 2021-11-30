open Funs

(********************************)
(* Part 1: High Order Functions *)
(********************************)

let contains_elem lst e = 
    fold (fun a b -> if b = e then true else a) false lst

let is_present lst x = 
    map (fun a -> if a = x then 1 else 0) lst

let count_occ lst target =
    fold (fun a b -> if b = target then a + 1 else a) 0 lst

let uniq lst = 
    fold (fun a b -> if contains_elem a b then a else b::a) [] lst

let assoc_list lst = 
    map (fun a -> (a, count_occ lst a)) (uniq lst)

let ap fns args = 
    fold (fun a b -> a @ (map b args)) [] fns
