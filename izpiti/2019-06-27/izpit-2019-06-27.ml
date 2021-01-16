(* 1.NALOGA ===========================================================*)
(* a-------------------------------------------------------------------*)

type complex = {re : float; im : float}

let complex_add z1 z2 = 
    {re = (z1.re +. z2.re); im = (z1.im +. z2.im) }

(* b--------------------------------------------------------------------*)
let complex_conjugate z = {re = z.re; im = z.im}

(* c---------------------------------------------------------------------*)
let rec list_apply_either pred f g list = match list with
    | [] -> []
    | x::xs -> 
        if pred x then
            (f x) :: (list_apply_either pred f g xs)
        else
            (g x) :: (list_apply_either pred f g xs)

(* d-------------------------------------------------------------------*)
let eval_poly list a =
    let rec aux acc a_n list = match list with
        | [] -> acc
        | x::xs -> aux (x * a_n + acc) (a_n * a) xs
    in
    aux 0 1 list

(* 2.NALOGA =============================================================*)

type lastnik = string

type vrt =
    | Obdelovan of lastnik
    | Oddan of lastnik * (vrt * vrt list)
    | Prost

(* a-------------------------------------------------------------------*)
let vrt_primer = Oddan ("Kovalevskay", (Obdelovan("Galois"), [Prost; Obdelovan("Laagrange")]) )

(* b------------------------------------------------------------------------------------*)
let obdelovalec_vrta vrt = match vrt with
    | Obdelovan(l) -> Some l 
    | _ -> None

(* c------------------------------------------------------------------------------------*)

(* !!!!! *)
let rec globina_oddajanja vrt = match vrt with
    | Prost -> 0
    | Obdelovan(_) -> 0
    | Oddan(_, (v, sez)) -> 
        (* seznam vseh moznih globin *)
        let seznam_globin = List.map globina_oddajanja sez in
        1 + List.fold_left max (globina_oddajanja v) seznam_globin

(* d-----------------------------------------------------------------------------------*)
let rec v_uporabi vrt = match vrt with
    | Obdelovan(_) -> true
    | Prost -> false
    | Oddan(_, (v, sez)) -> (v_uporabi v) || (List.fold_left (||) false (List.map v_uporabi sez))

(* druga moznost je da v zadnji vrstici uporabis List.exists *)

(* e-----------------------------------------------------------------------------------*)
let rec vsi_najmeniki vrt = match vrt with
    | Prost -> []
    | Obdelovan(l) -> [l]
    | Oddan(l, (v, sez)) ->
        let rec najemniki_sez sez = match sez with
            | [] -> []
            | x::xs -> (vsi_najmeniki x) @ (najemniki_sez xs)
        in
        [l] @ (vsi_najmeniki v) @ (najemniki_sez sez)

(* f------------------------------------------------------------------------------*)
let rec vsi_obdelovalci vrt = match vrt with
    | Prost -> []
    | Obdelovan(l) -> [l]
    | Oddan(l, (v, sez)) ->
        let rec najemniki_sez sez = match sez with
            | [] -> []
            | x::xs -> (vsi_obdelovalci x) @ (najemniki_sez xs)
        in
        (vsi_obdelovalci v) @ (najemniki_sez sez)