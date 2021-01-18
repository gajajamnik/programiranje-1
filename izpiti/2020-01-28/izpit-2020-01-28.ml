(* 1.NALOGA *)
(* a----------------------------------------------------------------------*)

let option_sum x y = match x with
    | None -> None
    | Some x ->
        match y with
        | None -> None
        | Some y -> Some (x + y)

(* b---------------------------------------------------------------------*)

let twostep_map f g h x =
    let (b, c) = f x in
    (g b, h c)

(* c---------------------------------------------------------------------*)

let function_repeat f list = 
    let rec aux acc list = match list with
        | [] -> acc
        | x:: xs ->
            let n = f x in
            let rec dodaj x n list = match n with
                | n' when n' <= 0 -> list
                | n'' ->  dodaj x (n - 1) (x::list)
            in
            aux (dodaj x n acc) xs
    in
    List.rev (aux [] list)

(* d---------------------------------------------------------------------*)

let rec iterate f pogoj z = 
    if pogoj z then z 
    else
        iterate f pogoj (f z)

(* 2.NALOGA *)
(* a----------------------------------------------------------------------*)

type 'a improved_list = 
    | Prazen
    | Vozlisce of 'a array * 'a improved_list

let test = Vozlisce([|1; 2; 20|], Vozlisce([|17; 19; 20; 30|], Vozlisce([|100|], Prazen)))

(* b----------------------------------------------------------------------*)
let rec count seznam = match seznam with
    | Prazen -> 0
    | Vozlisce(a, list) -> Array.length a + (count list) 

(* c------------------------------------------------------------------------*)
let rec nth i seznam = match seznam with
    | Prazen -> None
    | Vozlisce(a, list) -> 
        let l = Array.length a in
        if i > l - 1 then 
            nth (i - l) list
        else
            Some a.(i)

(* d---------------------------------------------------------------------------*)

(* ????????????????????? *)
let is_sorted seznam = 
    let rec aux acc zadnji seznam = match seznam with
        | Prazen -> acc
        | Vozlisce(a, list) ->
            let l = Array.length a + 1 in
            let rec urejen a i = 
                if i > l then true
                else if i = 0 then
                    (zadnji < a.(i)) && (urejen a (i + 1))
                else
                    (a.(i) < a.(i + 1)) && (urejen a (i + 1))
            in
            aux ((urejen a 0) && acc) a.(l) list
    in
    aux true neg_infinity seznam

(* e---------------------------------------------------------------------------*)
let rec update seznam i n = match seznam with
    | Prazen -> Prazen
    | Vozlisce(a, list) -> 
        let l = Array.length a in
        if i > l - 1 then 
            Vozlisce(a, update list (i - l) n)
        else
            let rec zamenjaj a j acc =
                if j > l - 1 then acc
                else if j != i then 
                    zamenjaj a (j + 1) (Array.append acc [|a.(j)|])
                else
                    zamenjaj a (j + 1) (Array.append acc [|n|]) 
            in

            Vozlisce(zamenjaj a 0 [||], list)