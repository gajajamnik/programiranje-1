(* ========== Vaja 2: Funkcijsko Programiranje  ========== *)

(*----------------------------------------------------------------------------*]
Namig: Definirajte pomožno funkcijo za obračanje seznamov.
[*----------------------------------------------------------------------------*)

let rec bad_reverse sez = match sez with
  | [] -> []
  | x :: xs -> bad_reverse xs @ [x]

(** primer z repno rekurzijo *)
let reverse list =
  let rec reverse_aux acc list =
    match list with
      | [] -> acc
      | x :: xs -> reverse_aux (x :: acc) xs
  in
  reverse_aux [] list

(*----------------------------------------------------------------------------*]
 Funkcija [repeat x n] vrne seznam [n] ponovitev vrednosti [x]. Za neprimerne
 vrednosti [n] funkcija vrne prazen seznam.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # repeat "A" 5;;
 - : string list = ["A"; "A"; "A"; "A"; "A"]
 # repeat "A" (-2);;
 - : string list = []
[*----------------------------------------------------------------------------*)

let rec repeat x n = 
  if n <= 0 then
    []
  else
    x :: repeat x (n - 1)

let repeat_rep x n =
  let rec aux acc n =
    if n <= 0 then
      acc
    else
      aux (x :: acc) (n - 1)
  in
  aux [] n

(*----------------------------------------------------------------------------*]
 Funkcija [range] sprejme število in vrne seznam vseh celih števil od 0 do
 vključno danega števila. Za neprimerne argumente funkcija vrne prazen seznam.
 Funkcija je repno rekurzivna.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # range 10;;
 - : int list = [0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10]
[*----------------------------------------------------------------------------*)

let range n =
  let rec aux n acc =
    if n < 0 then
      acc
    else
      aux (n - 1) (n :: acc)
  in
  aux n []

(*----------------------------------------------------------------------------*]
 Funkcija [map f list] sprejme seznam [list] oblike [x0; x1; x2; ...] in
 funkcijo [f] ter vrne seznam preslikanih vrednosti, torej
 [f x0; f x1; f x2; ...].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # let plus_two = (+) 2 in
   map plus_two [0; 1; 2; 3; 4];;
 - : int list = [2; 3; 4; 5; 6]
[*----------------------------------------------------------------------------*)

let rec map f list = match list with
  | [] -> []
  | x :: xs -> f x :: map f xs
 
(*----------------------------------------------------------------------------*]
 Funkcija [map_tlrec] je repno rekurzivna različica funkcije [map].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # let plus_two = (fun x -> x + 2) in
   map_tlrec plus_two [0; 1; 2; 3; 4];;
 - : int list = [2; 3; 4; 5; 6]
[*----------------------------------------------------------------------------*)

let map_tlrec f list = 
  let rec map_aux acc list = match list with
    | [] -> reverse acc                       (* !!! tu je nujno potreben REVERSE ker nalagamo iz leve strani *)
    | x :: xs -> map_aux (f x :: acc) xs
  in
  map_aux [] list

(*----------------------------------------------------------------------------*]
 Funkcija [mapi] je ekvivalentna python kodi:

  def mapi(f, list):
      mapi_list = []
      index = 0
      for x in list:
          mapi_list += [f(x, index)]
          index += 1
      return mapi_list

 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # mapi (+) [0; 0; 0; 2; 2; 2];;
 - : int list = [0; 1; 2; 5; 6; 7]
[*----------------------------------------------------------------------------*)

let rec mapi f list =
(* f je funkcija dveh argumentov*)
  let rec mapi_aux i acc list = match list with
    | [] -> reverse acc
    | x :: xs -> mapi_aux (i + 1) ((f x i) :: acc) xs  (* obvezno uporabis na seznamu xs *)
  in
  mapi_aux 0 [] list

(*----------------------------------------------------------------------------*]
 Funkcija [zip] sprejme dva seznama in vrne seznam parov istoležnih
 elementov podanih seznamov. Če seznama nista enake dolžine vrne napako.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # zip [1; 1; 1; 1] [0; 1; 2; 3];;
 - : (int * int) list = [(1, 0); (1, 1); (1, 2); (1, 3)]
 # zip [1; 1; 1; 1] [1; 2; 3; 4; 5];;
 Exception: Failure "Different lengths of input lists.".
[*----------------------------------------------------------------------------*)

let rec zip list1 list2 =
  match list1 with
  | x :: xs -> (
    match list2 with
      | y :: ys -> (x, y) :: zip xs ys
      | [] -> failwith "Different lenght of input lists")
  | [] -> (
    match list2 with
      | y :: ys -> failwith "Different lenght of input lists"
      | [] -> []
  )

let rec zip' list1 list2 =
  match (list1, list2) with
  | (x :: xs, y :: ys) -> (x, y) :: zip' xs ys
  | x :: xs, [] -> failwith "Different lenght of input lists"
  | [], y :: ys -> failwith"Different lenght of input lists"
  | [], [] -> []


(*----------------------------------------------------------------------------*]
 Funkcija [unzip] je inverz funkcije [zip], torej sprejme seznam parov
 [(x0, y0); (x1, y1); ...] in vrne par seznamov ([x0; x1; ...], [y0; y1; ...]).
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # unzip [(0,"a"); (1,"b"); (2,"c")];;
 - : int list * string list = ([0; 1; 2], ["a"; "b"; "c"])
[*----------------------------------------------------------------------------*)

let rec unzip = function
  | [] -> ([], [])
  | (x, y) :: xys ->
     let (xs, ys) = unzip xys in
     (x :: xs, y :: ys)

(*----------------------------------------------------------------------------*]
 Funkcija [unzip_tlrec] je repno rekurzivna različica funkcije [unzip].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # unzip_tlrec [(0,"a"); (1,"b"); (2,"c")];;
 - : int list * string list = ([0; 1; 2], ["a"; "b"; "c"])
[*----------------------------------------------------------------------------*)

let rec unzip_tlrec list = 
  let rec aux acc1 acc2 = function  (* akumulatorja sta lahko dva *)
    | [] -> (reverse acc1, reverse acc2)
    | (x, y) :: xys -> aux (x :: acc1)(y :: acc2) xys
  in
  aux [] [] list


(*----------------------------------------------------------------------------*]
 Funkcija [loop condition f x] naj se izvede kot python koda:

  def loop(condition, f, x):
      while condition(x):
          x = f(x)
      return x

 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # loop (fun x -> x < 10) ((+) 4) 4;;
 - : int = 12
[*----------------------------------------------------------------------------*)
let rec loop condition f x =
  if condition x then
    (* Izvedemo telo zanke. *)
    let x = f x in
    (* Ponovno izvedemo zanko. *)
    loop condition f x
  else
    (* Pogoj zanke ni izpolnjen, zato vrnemo vrednost. *)
    x


(* samostojno *)
let loop' condition f x = 
  let rec aux acc =
    if (condition acc) = true
      then aux (f acc)
    else
      acc
  in
  aux x 
  

(*----------------------------------------------------------------------------*]
 Funkcija [fold_left_no_acc f list] sprejme seznam [x0; x1; ...; xn] in
 funkcijo dveh argumentov [f] in vrne vrednost izračuna
 f(... (f (f x0 x1) x2) ... xn).
 V primeru seznama z manj kot dvema elementoma vrne napako.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # fold_left_no_acc (^) ["F"; "I"; "C"; "U"; "S"];;
 - : string = "FICUS"
[*----------------------------------------------------------------------------*)

let rec fold_left_no_acc f list = match list with
    | [] -> failwith "prekratek seznam"
    | [_] -> failwith "prekratek seznam"
    | x::y::xs -> fold_left_no_acc f ((f x y) :: xs)
(*----------------------------------------------------------------------------*]
 Funkcija [apply_sequence f x n] vrne seznam zaporednih uporab funkcije [f] na
 vrednosti [x] do vključno [n]-te uporabe, torej
 [x; f x; f (f x); ...; (f uporabljena n-krat na x)].
 Funkcija je repno rekurzivna.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # apply_sequence (fun x -> x * x) 2 5;;
 - : int list = [2; 4; 16; 256; 65536; 4294967296]
 # apply_sequence (fun x -> x * x) 2 (-5);;
 - : int list = []
[*----------------------------------------------------------------------------*)

let apply_sequence f x n =
  let rec aux acc x n =
    if n <= 0 
      then reverse acc
    else
      aux (x :: acc) (f x) (n - 1)
  in 
  aux [x] (f x) n
    

(*----------------------------------------------------------------------------*]
 Funkcija [filter f list] vrne seznam elementov [list], pri katerih funkcija [f]
 vrne vrednost [true].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # filter ((<)3) [0; 1; 2; 3; 4; 5];;
 - : int list = [4; 5]
[*----------------------------------------------------------------------------*)

let rec filter f list = match list with
  | [] -> []
  | x::xs -> 
    if f x = true
      then x :: (filter f xs)
    else
      filter f xs

(*----------------------------------------------------------------------------*]
 Funkcija [exists] sprejme seznam in funkcijo, ter vrne vrednost [true] čim
 obstaja element seznama, za katerega funkcija vrne [true] in [false] sicer.
 Funkcija je repno rekurzivna.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # exists ((<) 3) [0; 1; 2; 3; 4; 5];;
 - : bool = true
 # exists ((<) 8) [0; 1; 2; 3; 4; 5];;
 - : bool = false
[*----------------------------------------------------------------------------*)

let exists f list =
  let rec aux acc list = match list with
    | [] -> acc
    | x :: xs -> aux (acc || f x) xs
  in
  aux false list

(*----------------------------------------------------------------------------*]
 Funkcija [first f default list] vrne prvi element seznama, za katerega
 funkcija [f] vrne [true]. Če takšnega elementa ni, vrne [default].
 Funkcija je repno rekurzivna.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # first ((<) 3) 0 [1; 1; 2; 3; 5; 8];;
 - : int = 5
 # first ((<) 8) 0 [1; 1; 2; 3; 5; 8];;
 - : int = 0
[*----------------------------------------------------------------------------*)

let rec first f default list = match list with
  | [] -> default
  | x::xs -> 
    if f x = true 
      then x
    else
      first f default xs