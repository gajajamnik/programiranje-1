(* 1. naloga *)

let odstej_trojici (x1, y1, z1) (x2, y2, z2) = (x1 - x2, y1 - y2, z1 - z2)


(* List.init *)

let max_rezultat_do_n f n =
    let rec aux trenutni_max trenutni_i =
        if trenutni_i < 0 then trenutni_max (* smo prisli do konca*)
        else (
            let v = f trenutni_i in 
            aux (max v trenutni_max) (trenutni_i - 1)
        )
    in
    aux (f n) (n-1)

(* c- primer *)

(* ne repno rekurzivna:
let pocisti_seznam sez = match sez with
    | [] -> []
    | (Some x) :: xs -> x :: pocisti_seznam xs
    | _::xs -> pocisti_seznam xs
    *)


let pocisti_seznam sez =
    let rec aux acc list = match list with
    | [] -> acc
    | (Some x) :: xs -> aux (x :: acc) xs
    | _::xs -> aux acc xs
    in
    aux [] sez

(* d primer *)

let preveri_urejenost sez =
    let rec aux min_sodo max_liho sez = match sez with
    | [] -> true
    | x :: xs -> 
        if x mod 2 = 0 then x > min_sodo && aux x max_liho xs
        else x < max_liho && aux min_sodo x xs
    in
    (* guljfamo in postavimo meje tako *)
    aux Int.min_int Int.max_int sez


(* 2. naloga *)
(* a primer  [1; 2; [3; [4]; []]; [5]]  *)

type 'a gnezdenje =
    | Element of 'a
    | Podseznam of 'a gnezdenje list

let gnezdenje_primer = Podseznam [Element 1; Element 2; Podseznam([Element 3; Podseznam([Element 4]); Podseznam([])]); Podseznam([Element 5])]

(* b primer *)

let rec najvecja_globina g = match g with
    | Element _ -> 0
    | Podseznam xs -> 1 + (List.fold_left max 1 (List.map najvecja_globina xs))

let rec preslikaj f g = match g with
    | Element x -> Element (f x)
    | Podseznam xs -> Podseznam (List.map (preslikaj f) xs)

let rec splosci g = match g with
    | Element x -> [x]
    | Podseznam xs ->
        let splosceni = List.map splosci xs in
        List.fold_left (@) [] splosceni

let rec alternirajoci_konstruktorji l = match l with
    | [] -> true
    | [x] -> true
    | Element _ :: Podseznam p:: xs -> alternirajoci_konstruktorji((Podseznam p)::xs)
    | _ -> false

let zlozi_preko_gnezdenja f acc g 