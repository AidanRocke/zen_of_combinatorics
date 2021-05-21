(*https://mathworld.wolfram.com/e.html*)

(*One of the lesser known qualities of Euler's number is that it has
optimal radix economy.*)

let rec factorial = function
    | 0 | 1 -> 1
    | n -> n * factorial (n - 1);;

let euler = fun n -> 1. /. (float_of_int (factorial n));;

let rec euler_number n =
    if n < 1 then (euler 0)
    else euler n +. euler_number (n - 1);;

(*The relation between Euler and Archimedes' constant via Stirling's approximation:
https://mathworld.wolfram.com/StirlingsApproximation.html*)

let da_powah = fun n -> (float_of_int n)**(float_of_int n);;

let phi = fun n -> (factorial n |> float_of_int)**2.
                  /. (2*n |> float_of_int);;

let psi = fun n -> ((da_powah n) /. (exp(float_of_int n)))**2.;;

let archimedes = fun n -> (phi n) /. (psi n);;

(*Floating point arithmetic breaks down aroun n =20.*)
archimedes 20;;
