(* 1. NALOGA *)
let angle_between (x1, y1) (x2, y2) =
    let skalarni (x1, y1) (x2, y2) =
        x1 *. x2 +. y1 *. y2
    in
    let norma (x1, y1) =
        (x1 ** 2. +. y1 ** 2.) ** 0.5
    in
    Float.acos ((skalarni (x1, y1) (x2, y2)) /. (norma (x1, y1) *. norma (x2, y2)))


let list_to_triple list = match list with
    | [x; y; z] -> Some (x, y, z)
    | _ -> None

type counter = {lt : int; eq : int; gt : int}

let compare_with list a = 
    let rec aux acc list = match list with
    | [] -> acc
    | x::xs ->
        if x < a then {acc with lt = acc.lt + 1}
        else if x = a then {acc with eq = acc.eq + 1}
        else
            {acc with gt = acc.gt + 1}
    in
    aux {lt = 0; eq = 0; gt = 0} list
(*                 ^^^
    PAZI! ko definiras nek elemnt zapisnega tipa das = in ne :  *)



(* PRIDE DO STACK OVERFLOW!!! *)
let apply_all list = 
    let rec aux acc list = match list with
        | [] -> acc
        | f::fs -> aux (fun x -> (f (acc x))) fs
    in
    aux (fun x -> x) list

(*
let apply_all' list =
    let rec aux acc list = match list with
        | [] -> acc
        | f::fs -> aux (acc (fun x -> (f x))) fs
    in
    aux (fun x -> x) list
*)
let long_test = List.init 1000000 (fun _ -> (+) 1)


(*2.NALOGA*)

type xytree =
    | Xsplit of int * xytree * xytree
    | Ysplit of int * xytree * xytree
    | Elements of (int * int) list

let example = Xsplit (2, Ysplit(3, Elements[(0, 2); (1, 1)], Elements []),
                         Ysplit(2, Elements([(3, 1)]), 
                                    Xsplit(4, Elements [(4, 3)], Elements [])
                                )
                    )

let rec num_of_elements drevo =
    let rec tocke list = match list with
        | [] -> 0
        | x::xs -> 1 + (tocke xs)
    in
    match drevo with
    | Elements list -> tocke list
    | Xsplit (_, l, d) -> (num_of_elements l) + (num_of_elements d)
    | Ysplit (_, l, d) -> (num_of_elements l) + (num_of_elements d)