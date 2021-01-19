(* 1.NALOGA=============================================================*)
let uporabi f x = f x

let ibaropu x f = f x

let zacetnih n list =
    let rec aux acc n list = match list with
        | [] -> None
        | x::xs when n > 0 -> aux (x::acc) (n - 1) xs
        | x::xs -> Some (List.rev acc)
    in
    aux [] n list

(* 2.NALOGA=============================================================*)
type 'a neprazen_sez = 
    | Konec of 'a 
    | Sestavljen of 'a * 'a neprazen_sez

let primer = Sestavljen(2, Sestavljen(6, Sestavljen(1, Konec 3)))

let prvi seznam = match seznam with
    | Konec x -> x 
    | Sestavljen (x, _) -> x 

let rec zadnji seznam = match seznam with
    | Konec x -> x
    | Sestavljen (_, l) -> zadnji l

let rec pretvori_v_seznam seznam = match seznam with
    | Konec x -> [x]
    | Sestavljen (x, l) -> x :: (pretvori_v_seznam l)

let rec zlozi f a seznam = match seznam with
    | Konec x -> f a x
    | Sestavljen (x, l) -> zlozi f (f a x) l