(* 1.NALOGA=============================================================*)

let rec izpisi_vsa_stevila list = match list with
    | [] -> ()
    | x::xs -> print_int x; izpisi_vsa_stevila xs

let rec map2_opt f list1 list2 =
    let rec aux acc list1 list2 = match (list1, list2) with
        | [], [] -> Some (List.rev acc)
        | [], _ -> None
        | _, [] -> None
        | x::xs, y::ys -> aux ((f x y)::acc) xs ys
    in 
    aux [] list1 list2

(* 2.NALOGA =============================================================*)

type filter_tree =
    | List of int list
    | Vozlisce of filter_tree * int * filter_tree

let primer : filter_tree = Vozlisce(Vozlisce(List [1], 5, List []), 10, Vozlisce(List [], 15, List [19; 20]))


let rec vstavi n tree = match tree with
    | List l -> List (n::l)
    | Vozlisce(l, x, d) ->
        if n <= x then Vozlisce(vstavi n l, x, d)
        else Vozlisce(l, x, vstavi n d)

let rec vstavi_seznam list tree = match list with
    | [] -> tree
    | x::xs -> vstavi_seznam xs (vstavi x tree)

let rec urejen tree = match tree with
    | List sez -> true
    | Vozlisce(List l, x, List d) ->
        let rec vsi_manjsi x sez = match sez with
            | [] -> true
            | y::ys -> 
                if y <= x && (vsi_manjsi x ys) then true
                else false
        in
        let rec vsi_vecji x sez = match sez with
            | [] -> true
            | y::ys -> 
                if y > x && (vsi_vecji x ys) then true
                else false
        in
        if (vsi_manjsi x l) && (vsi_vecji x d) then true
        else false
    | Vozlisce(l_drevo, x, d_drevo) -> (urejen l_drevo) && (urejen d_drevo)



