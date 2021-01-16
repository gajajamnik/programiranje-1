(* 1. naloga *)
(* a---------------------------------------------------------------------*)
let podvoji_vsoto x y = 2 * (x + y)

(* b---------------------------------------------------------------------*)
let povsod_vecji (x1, y1, z1) (x2, y2, z2) = (x1 > x2) && (y1 > y2) && (z1 > z2)

(* c--------------------------------------------------------------------*)
let uporabi_ce_lahko f x =
    if x = None then None
    else f x 

(* BOLJSA RESITEV !!! *)

let uporabi_ce_lahko f x = match x with
    | None -> None
    | Some x -> Some (f x)

(* d----------------------------------------------------------------*)
let pojavi_dvakrat x list =
    let rec ponovitve x list = match list with
        | [] -> 0
        | y::ys -> 
            if y = x then 
                1 + (ponovitve x ys)
            else
                (ponovitve x ys)
    in
    2 = (ponovitve x list)


(* e----------------------------------------------------------------*)
let izracunaj_v_tocki x list =
    let rec aux acc list = match list with
        | [] -> List.rev acc                (* PAZII: obvezno zamenjas vrstni red *)
        | f::fs -> aux ((f x)::acc) fs
    in
    aux [] list

(* f--------------------------------------------------------------------*)
let eksponent x p = 
    let rec aux acc p = match p with
        | 0 -> acc
        | _ -> aux (x * acc) (p - 1)
    in
    aux 1 p

(* 2.NALOGA =============================================================*)
(* a---------------------------------------------------------------------*)
type 'a mn_drevo =
    | Prazno
    | Vozlisce of 'a mn_drevo * 'a * int * 'a mn_drevo

let primer = Vozlisce(Vozlisce(Prazno, 1, 3, Prazno), 2, 1, Vozlisce(Vozlisce(Prazno, 4, 1, Prazno), 5, 1, Vozlisce(Prazno, 8, 2, Prazno)))

(* b----------------------------------------------------------------------*)
let rec vstavi drevo x = match drevo with
    | Prazno -> Vozlisce (Prazno, x, 1, Prazno)
    | Vozlisce(l, y, n, d) ->
        if x < y then 
            Vozlisce(vstavi l x, y , n, d)
        else if x > y then 
            Vozlisce(l, y, n, vstavi d x)
        else
            Vozlisce(l, y, n + 1, d)

(* c--------------------------------------------------------------------*)
let multimnozica_iz_seznama list =
    let rec aux acc list = match list with   
        | [] -> acc
        | x::xs -> aux (vstavi acc x) xs
    in
    aux Prazno list

(* d--------------------------------------------------------------------*)
let rec velikost_multimnozice drevo = match drevo with
    | Prazno -> 0
    | Vozlisce(l, x, n, d) -> n + (velikost_multimnozice l) + (velikost_multimnozice d)

(* e---------------------------------------------------------------------*)
let dodaj x n list = 
    let rec aux acc n = match n with
        | 0 -> acc
        | n' -> aux (x:: acc) (n' - 1)
    in
    aux list n

let seznam_iz_multimnozice drevo = 
    let rec aux acc drevo = match drevo with
    | Prazno -> acc
    | Vozlisce(l, x, n, d) -> (aux (dodaj x n acc) l) @ (aux acc d)
    in
    aux [] drevo
