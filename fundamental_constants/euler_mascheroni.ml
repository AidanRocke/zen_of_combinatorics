(*Exponential of the Euler-Mascheroni constant: https://mathworld.wolfram.com/Euler-MascheroniConstant.html
leads to a natural reformulation of Mertens' third theorem.*)

let rec harmonic = function
    | 1 -> 1.
    | n -> 1. /. (float_of_int n) +. harmonic (n - 1);;

let exp_em = fun n -> exp(harmonic n) /. (float_of_int n);;
