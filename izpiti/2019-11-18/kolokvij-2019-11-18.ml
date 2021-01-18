(* =================== *)
(* 1. naloga: funkcije *)
(* =================== *)

let is_root x y =
    if y = x * x && x > 0 then true
    else false

let pack3 i1 i2 i3 = (i1, i2, i3)

let sum_if_not pred list =
    let rec aux acc list = match list with
        | [] -> acc
        | x::xs -> 
            if pred x then aux acc xs
            else aux (x + acc) xs
    in
    aux 0 list

let apply f_list x_list = 
    let rec aux acc x_list = match x_list with
        | [] -> acc
        | x::xs -> 
            let rec aux' acc' f_list = match f_list with
            | [] -> acc'
            | f::fs -> aux' ((f x)::acc') fs
            in
            (aux ((List.rev (aux' [] f_list))::acc) xs)

    in
    List.rev (aux [] x_list)

(* ======================================= *)
(* 2. naloga: podatkovni tipi in rekurzija *)
(* ======================================= *)

type vrsta_srecanja = 
    | Predavanje
    | Vaje 

type srecanje = {ime : string; vrsta : vrsta_srecanja; st_ur : int}

type urnik = (srecanje list) list

let vaje = {ime = "Analiza 2a"; vrsta = Vaje; st_ur = 3} 

let predavanja = {ime = "Programiranje 1"; vrsta = Predavanje; st_ur = 2}

let urnik_profesor = [[{ime = "Predmet 1"; vrsta = Vaje; st_ur = 2}];
                        [];
                        [{ime = "Predmet 2"; vrsta = Predavanje; st_ur = 1}];
                        [];
                        [];
                        [{ime = "Predmet 3"; vrsta = Vaje; st_ur = 1}]]

let rec je_preobremenjen urnik = match urnik with
    | [] -> false
    | d::ds -> 
        let rec st_ur d = match d with
            | [] -> 0
            | x::xs -> x.st_ur + st_ur xs
        in
        if (st_ur d) <= 4 then je_preobremenjen ds
        else
            true

let bogastvo urnik = 
    let rec aux acc urnik = match urnik with
        | [] -> acc
        | d::ds ->
            let rec dnevna_placa acc' d = match d with
            | [] -> acc'
            | x::xs -> 
                if x.vrsta = Vaje then 
                    dnevna_placa (x.st_ur + acc') xs
                else
                    dnevna_placa (x.st_ur * 2 + acc') xs
            in
            aux (acc + dnevna_placa 0 d) ds

    in
    aux 0 urnik
