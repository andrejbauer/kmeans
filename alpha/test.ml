(* Test na podatkih *)
open Frame
open Pairings
open Aggregates
open Common

let parse lst = 
  let f = List.nth lst in
  ((f 0,
   float_of_string (f 1),
   float_of_string (f 2),
   f 3,
   f 4,
   f 5,
   f 6,
   float_of_string (f 7),
   f 8,
   f 9,
   float_of_string (f 10),
   f 11,
   f 12,
   float_of_string (f 13),
   float_of_string (f 14)),
   f 15)


module Str = Discrete 
  (struct
    type t = bytes
    let equal = (=)
  end)

  
module Att = Product15 (Str) (CutFloat) (CutFloat) (Str) (Str) (Str) (Str) (CutFloat) (Str) (Str) (CutFloat) (Str) (Str) (CutFloat) (CutFloat) (Avg)

module Cla = Str

module Ktest = Kmeans (Att) (Cla)

let splitdata () = 
  let data = Csv.load "crx.data" in
  let filter x = Random.bool () in
  let dataset = List.map parse data in
  List.partition filter dataset
  
let mean d1 d2 sez =
  let filter x = (snd x) = "+" in
  let (p,m) = List.partition filter sez in
  if (List.length p) > (List.length m) then "+" else "-"
  


let natancnost = 
  let (train,test) = splitdata () in
  let resitve = List.map snd test in
  let pred x = Ktest.predict train 1 mean (fst x) in
  let rezultati = List.map pred test in
  let add a b c = if a = b then c+.1. else c in
  let hits = List.fold_right2 add resitve rezultati 0. in
  print_float (hits /. (float_of_int (List.length test)));
  print_newline ()
