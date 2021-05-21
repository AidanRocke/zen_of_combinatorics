(*https://mathworld.wolfram.com/KhinchinsConstant.html*)

let aux_K = fun (x: float) -> (1. +. (1. /. (x *.(x +. 2.))))**((log x) /. (log 2.));;

let rec khinchin n =
    if n <= 1 then 1.
    else (aux_K (float_of_int n)) *. khinchin (n - 1);;
