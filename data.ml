open Funs

(*************************************)
(* Part 2: Three-Way Search Tree *)
(*************************************)

type int_tree =
  | IntLeaf
  | IntNode of int * int option * int_tree * int_tree * int_tree 

let empty_int_tree = IntLeaf

let rec int_insert x t =
  match t with
    IntLeaf -> IntNode (x, None, IntLeaf, IntLeaf, IntLeaf)
  | IntNode (a, Some b, l, m, r) when x < a -> IntNode(a, Some b, int_insert x l, m , r)
  | IntNode (a, Some b, l, m, r) when x > b -> IntNode(a, Some b, l, m , int_insert x r)
  | IntNode (a, Some b, l, m, r) when x > a && x < b -> IntNode(a, Some b, l, int_insert x m , r)
  | IntNode (a, Some b, l, m, r) -> t
  | IntNode (a, None, l, m, r) when x < a -> IntNode(x, Some a, l, m , r)
  | IntNode (a, None, l, m, r) when x > a -> IntNode(a, Some x, l, m, r)
  | IntNode (a, None, l, m, r) -> t

let rec int_mem x t =
  match t with
    IntLeaf -> false
  | IntNode (a, Some b, l, m, r) when x < a -> int_mem x l
  | IntNode (a, Some b, l, m, r) when x > b -> int_mem x r
  | IntNode (a, Some b, l, m, r) when x > a && x < b -> int_mem x m
  | IntNode (a, Some b, l, m, r) -> true
  | IntNode (a, None, l, m, r) when x = a -> true
  | IntNode (a, None, l, m, r) -> false

let rec int_size t =
  match t with 
    IntLeaf -> 0
  | IntNode (a, Some b, l, m, r) -> 2 + int_size l + int_size m + int_size r
  | IntNode (a, None, l, m, r) -> 1 + int_size l + int_size m + int_size r

let rec aux_max n t =
  match t with
    IntLeaf -> n 
  | IntNode (a, None, l, m, r) -> a
  | IntNode (a, Some b, l, m, r) -> aux_max b r 


let rec int_max t =
  match t with
    IntLeaf -> invalid_arg "int max"
  | IntNode (a, Some b, l, m, r) -> aux_max b t
  | IntNode (a, None, l, m, r) -> a 

(*******************************)
(* Part 3: Three-Way Search Tree-Based Map *)
(*******************************)

type 'a tree_map =
  | MapLeaf
  | MapNode of (int * 'a) * (int * 'a) option * 'a tree_map * 'a tree_map * 'a tree_map

let empty_tree_map = MapLeaf

let rec map_put k v t = 
  match t with 
    MapLeaf -> MapNode ((k, v), None, MapLeaf, MapLeaf, MapLeaf)
  | MapNode ((a, b), Some (c, d), l, m, r) when k < a ->  MapNode ((a, b), Some (c, d), map_put k v l, m, r)
  | MapNode ((a, b), Some (c, d), l, m, r) when k > c ->  MapNode ((a, b), Some (c, d), l, m, map_put k v r)
  | MapNode ((a, b), Some (c, d), l, m, r) when k > a && k < c ->  MapNode ((a, b), Some (c, d), l, map_put k v m, r)
  | MapNode ((a, b), Some (c, d), l, m, r) -> invalid_arg "map_put"
  | MapNode ((a, b), None, l, m, r) when k < a -> MapNode ((k, v), Some (a, b), l, m, r)
  | MapNode ((a, b), None, l, m, r) when k > a -> MapNode ((a, b), Some (k, v), l, m, r)
  | MapNode ((a, b), None, l, m, r) -> invalid_arg "map_put"

let rec map_contains k t = 
  match t with
    MapLeaf -> false
  | MapNode ((a, b), Some (c, d), l, m, r) when k < a -> map_contains k l
  | MapNode ((a, b), Some (c, d), l, m, r) when k > c -> map_contains k r
  | MapNode ((a, b), Some (c, d), l, m, r) when k < c && k > a -> map_contains k m
  | MapNode ((a, b), Some (c, d), l, m, r) -> true
  | MapNode ((a, b), None, l, m, r) when k = a -> true
  | MapNode ((a, b), None, l, m, r) -> false

let rec map_get k t =
  match t with
    MapLeaf -> invalid_arg "map_get"
  | MapNode ((a, b), Some (c, d), l, m, r) when k = c -> d
  | MapNode ((a, b), Some (c, d), l, m, r) when k = a -> b 
  | MapNode ((a, b), Some (c, d), l, m, r) when k < a -> map_get k l 
  | MapNode ((a, b), Some (c, d), l, m, r) when k > c -> map_get k r
  | MapNode ((a, b), Some (c, d), l, m, r) when k < c && k > a -> map_get k m 
  | MapNode ((a, b), Some (c, d), l, m, r) -> invalid_arg "map_get"
  | MapNode ((a, b), None, l, m, r) when k = a -> b
  | MapNode ((a, b), None, l, m, r) -> invalid_arg "map_get"

(***************************)
(* Part 4: Variable Lookup *)
(***************************)

(* Modify the next line to your intended type *)

type lookup_table = 
  | NoScope
  | Scope of string list * int list * lookup_table

let empty_table = NoScope

let push_scope (table:lookup_table) : lookup_table = 
  match table with
    NoScope -> Scope ([], [], NoScope)
  | Scope (n, v, p) -> Scope ([], [], table)

let pop_scope (table:lookup_table) : lookup_table =
  match table with
    NoScope -> failwith "No scopes remain!"
  | Scope (n, v, p) -> p

let rec has_name name nameLst = 
  fold (fun a b -> if b = name then true else a) false nameLst

let rec get_value index valueLst = 
  match valueLst with 
    [] -> failwith "Variable not found!"
  | h::t -> if index = 0 then h else (get_value (index - 1) t)

let rec get_index name nameLst = 
  match nameLst with 
    [] -> failwith "Variable not found!"
  | h::t -> if name = h then 0 else 1 + (get_index name t)

let add_var name value (table:lookup_table) : lookup_table =
  match table with 
    NoScope -> failwith "There are no scopes to add a variable to!"
  | Scope (n, v, p) when (has_name name n) -> failwith "Duplicate variable binding in scope!"
  | Scope (n, v, p) -> Scope ((n @ [name]), (v @ [value]), p)

let rec lookup name (table:lookup_table) =
  match table with
    NoScope -> failwith "Variable not found!"
  | Scope (n, v, p) -> if (has_name name n) then get_value (get_index name n) v else lookup name p 
  