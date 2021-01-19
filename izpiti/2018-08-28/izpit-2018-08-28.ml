(* 1.NALOGA *)
(* a----------------------------------------------------------------------*)
let razlika_kvadratov x y = (x + y)*(x + y) - (x * x + y * y)

(* b-----------------------------------------------------------------------*)

let uporabi_na_paru f (x, y) = (f x, f y)

(* c----------------------------------------------------------------------*)

let rec ponovi_seznam n sez = match sez with
    | [] -> []
    | l -> 
        if n <= 0 then []
        else l @ (ponovi_seznam (n - 1) sez)

(* d----------------------------------------------------------------------*)

let razdeli sez =
    let rec aux (acc_neg, acc_poz) sez = match sez with
        | [] -> acc_neg, acc_poz
        | x::xs -> 
            if x < 0 then aux (x::acc_neg, acc_poz) xs
            else aux (acc_neg, x::acc_poz) xs
    in
    aux ([], []) sez

(* 2.NALOGA ==============================================================*)

type tree =
    | Prazno
    | Vozlisce of tree * int * tree

let monotona_pot drevo = None

(* 3.NALOGA ============================================================*)
type 'a veriga = 
    | Filter of ('a -> bool) * 'a list * 'a veriga
    | Ostalo of 'a list

let test : int veriga = Filter ((>) 0, [], Filter((>) 10, [], Ostalo([])))

let rec vstavi x veriga = match veriga with
    | Filter (f, l, v) when f x -> Filter(f, x::l, v)
    | Filter (f, l, v) -> Filter(f, l, vstavi x v)
    | Ostalo (l) -> Ostalo (x::l)

let veriga = vstavi (-5) (vstavi 7 ( vstavi 100 (vstavi (-7) (vstavi 2 test))));;

let izprazni_filtre veriga = 
    let rec aux (acc1, acc2) veriga = match veriga with
    | Ostalo (l) -> acc1, (acc2 @ l)
    | Filter (f, l, v) -> aux (Filter(f, [], acc1), acc2 @ l) v
    in 
    aux (Ostalo([]), []) veriga

let dodaj_filter f veriga = 
    let prazna, elementi = izprazni_filtre veriga in
    let nov_filter = Filter (f, [], prazna) in
    
    let rec dodaj elementi prazna_veriga = match elementi with
    | [] -> prazna_veriga
    | x::xs -> dodaj xs (vstavi x prazna_veriga)
    in
    dodaj elementi nov_filter

let rec dodaj elementi prazna_veriga = match elementi with
    | [] -> prazna_veriga
    | x::xs -> dodaj xs (vstavi x prazna_veriga)