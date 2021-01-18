(* -------- 1 -------- *)
let vsota list =
    let rec aux acc list = match list with
        | [] -> acc
        | x::xs -> aux (x + acc) xs
    in
    aux 0 list
(* -------- 2 -------- *)
let rec urejen list = match list with
    | [] -> true
    | [x] -> true
    | x::y::xs ->
        if x <= y then urejen (y::xs)
        else false
(* -------- 3 -------- *)
let rec vstavi x list = match list with
    | [] -> [x]
    | y::ys ->
        if y < x then y::(vstavi x ys)
        else x::y::ys

let uredi list =
    let rec aux acc list = match list with
        | [] -> acc
        | x::xs -> aux (vstavi x acc) xs
    in
    aux [] list


(* -------- 4 -------- *)
let rec vstavi' cmp x list = match list with
    | [] -> [x]
    | y::ys ->
        if cmp x y then x::y::ys
        else y::(vstavi' cmp x ys)

let rec uredi' cmp list = match list with
    | [] -> []
    | x::xs -> vstavi' cmp x (uredi' cmp xs) 

(* -------- 5 -------- *)
type priority =
    | Top
    | Group of int

type status =
    | Staff
    | Passenger of priority

type flyer = { status : status ; name : string }

let flyers = [ {status = Staff; name = "Quinn"}
             ; {status = Passenger (Group 0); name = "Xiao"}
             ; {status = Passenger Top; name = "Jaina"}
             ; {status = Passenger (Group 1000); name = "Aleks"}
             ; {status = Passenger (Group 1000); name = "Robin"}
             ; {status = Staff; name = "Alan"}
             ]


(* -------- 6 -------- *)
let cmp_flyers x y = match x.status, y.status with
    | Staff, _ -> true
    | _, Staff -> false
    | Passenger(Top), _ -> true
    | Passenger(_), Passenger(Top) -> false
    | Passenger(Group n1), Passenger(Group n2) -> n1 < n2

let vkrcanje list = uredi' cmp_flyers list
(* -------- 7 -------- *)
