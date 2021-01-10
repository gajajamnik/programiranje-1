(* ========== Vaja 3: Definicije Tipov  ========== *)

(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 Pri modeliranju denarja ponavadi uporabljamo racionalna števila. Problemi se
 pojavijo, ko uvedemo različne valute.
 Oglejmo si dva pristopa k izboljšavi varnosti pri uporabi valut.
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)

(* SLAB NACIN *)
type euro = float
type dollar = float

let my_euros : euro = 34.56
let my_euros = (34.56 : euro)

let euro_to_dollar (euros : euro) : dollar = euros *. 2.5
(*                           ^         ^
                     tip argumenta    tip rezultata
 *)

(*----------------------------------------------------------------------------*]
 Definirajte tipa [euro] in [dollar], kjer ima vsak od tipov zgolj en
 konstruktor, ki sprejme racionalno število.
 Nato napišite funkciji [euro_to_dollar] in [dollar_to_euro], ki primerno
 pretvarjata valuti (točne vrednosti pridobite na internetu ali pa si jih
 izmislite).

 Namig: Občudujte informativnost tipov funkcij.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # dollar_to_euro;;
 - : dollar -> euro = <fun>
 # dollar_to_euro (Dollar 0.5);;
 - : euro = Euro 0.4305
[*----------------------------------------------------------------------------*)

type euro = Euro of float
type dollar = Dollar of float

let dollar_to_euro dollar =
       match dollar with
       | Dollar d -> Euro (0.7 *. d)

let euro_to_dollar (Euro e) = Dollar (1.2 *. e)


(*----------------------------------------------------------------------------*]
 Definirajte tip [currency] kot en VSOTNI TIP z konstruktorji za jen, funt
 in švedsko krono. Nato napišite funkcijo [to_pound], ki primerno pretvori
 valuto tipa [currency] v funte.

 Namig: V tip dodajte še švicarske franke in se navdušite nad dejstvom, da vas
        Ocaml sam opozori, da je potrebno popraviti funkcijo [to_pound].

        OBEZNO MORAS UPORABIT VSE ELEMENTE VSOTE - TOREJ VSE TIPE !!!

 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # to_pound (Yen 100.);;
 - : currency = Pound 0.007
[*----------------------------------------------------------------------------*)

type currency = Yen of float | Pound of float | Krone of float

let my_yen = Yen 1987.12

let to_pound = function
       | Yen y -> Pound (y *. 0.05)
       | Pound p -> Pound p               (* OBVEZNO SE SKLICES TUDI NA FUNTE SAME*)
       | Krone k -> Pound (k *. 2.7)


(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 Želimo uporabljati sezname, ki hranijo tako cela števila kot tudi logične
 vrednosti. To bi lahko rešili tako da uvedemo nov tip, ki predstavlja celo
 število ali logično vrednost, v nadaljevanju pa bomo raje konstruirali nov tip
 seznamov.

 Spomnimo se, da lahko tip [list] predstavimo s konstruktorjem za prazen seznam
 [Nil] (oz. [] v Ocamlu) in pa konstruktorjem za člen [Cons(x, xs)] (oz.
 x :: xs v Ocamlu).
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)

(* slabši naćin:

type int_or_bool = Int of int | Bool of bool
type intbool_list = int_or_bool list 

*)

(*----------------------------------------------------------------------------*]
 Definirajte tip [intbool_list] z konstruktorji za:
  1.) prazen seznam,
  2.) člen z celoštevilsko vrednostjo,
  3.) člen z logično vrednostjo.

 Nato napišite testni primer, ki bi predstavljal "[5; true; false; 7]".
[*----------------------------------------------------------------------------*)

type intbool_list = 
       | Nil 
       | IntCons of int * intbool_list 
       | BoolCons of bool * intbool_list

let primer = IntCons (5, BoolCons (true, BoolCons(false, IntCons(7, Nil))))

(*----------------------------------------------------------------------------*]
 Funkcija [intbool_map f_int f_bool ib_list] preslika vrednosti [ib_list] v nov
 [intbool_list] seznam, kjer na elementih uporabi primerno od funkcij [f_int]
 oz. [f_bool].
[*----------------------------------------------------------------------------*)

let rec intbool_map f_int f_bool ib_list = match ib_list with
       | Nil -> Nil
       | IntCons(x, xs) -> IntCons(f_int x, intbool_map f_int f_bool xs)
       | BoolCons(x, xs) -> BoolCons(f_bool x, intbool_map f_int f_bool xs)


(*----------------------------------------------------------------------------*]
 Funkcija [intbool_reverse] obrne vrstni red elementov [intbool_list] seznama.
 Funkcija je repno rekurzivna.
[*----------------------------------------------------------------------------*)

let intbool_reverse intbool_list =
        let rec aux acc intbool_list = match intbool_list with
              | Nil -> acc
              | IntCons(x, xs) -> aux (IntCons(x, acc)) xs
              | BoolCons(x, xs) -> aux (BoolCons(x, acc)) xs
       in
       aux Nil intbool_list


(*----------------------------------------------------------------------------*]
 Funkcija [intbool_separate ib_list] loči vrednosti [ib_list] v par [list]
 seznamov, kjer prvi vsebuje vse celoštevilske vrednosti, drugi pa vse logične
 vrednosti. Funkcija je repno rekurzivna in ohranja vrstni red elementov.
[*----------------------------------------------------------------------------*)

let intbool_separate ib_list =
       let rec aux acc1 acc2 ib_list = match ib_list with
              | Nil ->  acc1, acc2
              | IntCons(x, xs) -> aux (x::acc1) acc2 xs
              | BoolCons(x, xs) -> aux acc1 (x::acc2) xs
       in
       aux [] [] (intbool_reverse ib_list)


(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 Določeni ste bili za vzdrževalca baze podatkov za svetovno priznano čarodejsko
 akademijo "Effemef". Vaša naloga je konstruirati sistem, ki bo omogočil
 pregledno hranjenje podatkov.
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)

(*----------------------------------------------------------------------------*]
 Čarodeje razvrščamo glede na vrsto magije, ki se ji posvečajo. Definirajte tip
 [magic], ki loči med magijo ognja, magijo ledu in magijo arkane oz. fire,
 frost in arcane.

 Ko se čarodej zaposli na akademiji, se usmeri v zgodovino, poučevanje ali
 raziskovanje oz. historian, teacher in researcher. Definirajte tip
 [specialisation], ki loči med temi zaposlitvami.
[*----------------------------------------------------------------------------*)

type magic = Fire | Frost | Arcane
type specialisation = Historian | Teacher | Researcher

(*----------------------------------------------------------------------------*]
 Vsak od čarodejev začne kot začetnik, nato na neki točki postane študent,
 na koncu pa SE lahko tudi zaposli.
 Definirajte tip [status], ki določa ali je čarodej:
  a.) začetnik [Newbie],
  b.) študent [Student] (in kateri vrsti magije pripada in koliko časa študira),
  c.) zaposlen [Employed] (in vrsto magije in specializacijo).

 Nato definirajte zapisni tip [wizard] z poljem za ime in poljem za trenuten
 status.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # professor;;
 - : wizard = {name = "Matija"; status = Employed (Fire, Teacher)}
[*----------------------------------------------------------------------------*)
type years = int


(* TIP VSOTA*)
type status =
       | Newbie
       | Student of magic * years
       | Employed of magic * specialisation

(*ZAPISNI TIP *)
type wizard = {name : string; status : status}

(* DEFINICIJA ELEMENTA ZAPISNEGA TIPA *)
let professor = {name = "Matija"; status = Employed (Fire, Teacher)}
let student = {name = "Gaja"; status = Student (Arcane, 2)}

(*----------------------------------------------------------------------------*]
 Želimo prešteti koliko uporabnikov posamezne od vrst magije imamo na akademiji.
 Definirajte zapisni tip [magic_counter], ki v posameznem polju hrani število
 uporabnikov magije.
 Nato definirajte funkcijo [update counter magic], ki vrne nov števec s
 posodobljenim poljem glede na vrednost [magic].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # update {fire = 1; frost = 1; arcane = 1} Arcane;;
 - : magic_counter = {fire = 1; frost = 1; arcane = 2}
[*----------------------------------------------------------------------------*)

type magic_counter = {fire : int; frost : int; arcane : int}

(* DVA NACINA SPREMINJAJANJA "VREDNOSTI" V ZAPISENM TIU *)

let update magic_counter magic = match magic with
       (* 1.NACIN *)
       | Arcane -> {arcane = magic_counter.arcane + 1; fire = magic_counter.fire; frost = magic_counter.frost}
       (* 2. NACIN *)
       | Fire -> {magic_counter with fire = magic_counter.fire + 1}
       | Frost -> {magic_counter with frost = magic_counter.frost + 1}

(*----------------------------------------------------------------------------*]
 Funkcija [count_magic] sprejme seznam čarodejev in vrne števec uporabnikov
 različnih vrst magij.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # count_magic [professor; professor; professor];;
 - : magic_counter = {fire = 3; frost = 0; arcane = 0}
[*----------------------------------------------------------------------------*)

let rec count_magic list = match list with
       | [] -> {arcane = 0; fire = 0; frost = 0}
       | x::xs ->
              let status = x.status in
              match status with
              | Newbie -> count_magic xs
              | Student (mag, _) -> update (count_magic xs) mag
              | Employed (mag', _) -> update (count_magic xs) mag'

let count_magic' list =
       let rec aux acc list = match list with
              | [] -> acc
              | x::xs ->
                     let status = x.status in
                     match status with
                     | Newbie -> aux acc xs
                     | Student (mag, _) -> aux (update acc mag) xs
                     | Employed (mag', _) -> aux (update acc mag') xs
       in
       aux {arcane = 0; fire = 0; frost = 0} list

(*----------------------------------------------------------------------------*]
 Želimo poiskati primernega kandidata za delovni razpis. Študent lahko postane
 zgodovinar po vsaj treh letih študija, raziskovalec po vsaj štirih letih
 študija in učitelj po vsaj petih letih študija.
 Funkcija [find_candidate magic specialisation wizard_list] poišče prvega
 primernega kandidata na seznamu čarodejev in vrne njegovo ime, čim ustreza
 zahtevam za [specialisation] in študira vrsto [magic]. V primeru, da ni
 primernega kandidata, funkcija vrne [None].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # let jaina = {name = "Jaina"; status = Student (Frost, 4)};;
 # find_candidate Frost Researcher [professor; jaina];;
 - : string option = Some "Jaina"
[*----------------------------------------------------------------------------*)

let rec find_candidate magic spec list = 
       let year = match spec with
       | Researcher -> 4
       | Historian -> 3
       | Teacher -> 5
       in
       match list with
       | [] -> None
       | {name; status}::xs ->
              match status with
              | Student (m, y) when m = magic && y >= year -> Some name
              | _ -> find_candidate magic spec xs
