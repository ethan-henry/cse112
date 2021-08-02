(* $Id: interp.ml,v 1.18 2021-01-29 11:08:27-08 - - $ *)

let rec filter less arr retarr = match arr with
| [] -> retarr
| x::_ when less x -> item::retarr
| x::xs -> insert less xs;;
