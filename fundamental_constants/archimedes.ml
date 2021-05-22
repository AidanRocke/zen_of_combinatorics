(*Monte Carlo method.*)
let mc_pi n =
  let square = fun x -> x *. x in

  (*Computing the norm of a vector.*)
  let norm = fun x -> List.map square x in

  (*Summing two lists of equal size.*)
  let sum_lists a b = List.map2(fun x y -> x +. y) a b in

  (*Check that point is inside circle.*)
  let in_circle xs = List.filter (fun x -> x <= 1.0) xs in

  let rec gen size = List.init size (fun _ -> Random.float 1.0) in

    (*Generate radii.*)
    let a, b = gen n |> norm, gen n |> norm in
    let c = sum_lists a b in

    (*Use the m/n = pi/4 approximation.*)
    let approx = 4. *. (float_of_int (List.length (in_circle c)))
                 /. (float_of_int (List.length c)) in

  approx
;;

(*The Newton-Euler formula: https://mathworld.wolfram.com/PiFormulas.html*)
let rec factorial = function
    | 0 | 1 -> 1
    | n -> n * factorial (n - 1);;

let da_powah = fun n -> 2.**(float_of_int n);;

let square = fun x -> x *. x;;

let newton = fun n -> (da_powah (n+1) *. (factorial n |> float_of_int |> square))
                      /. (factorial (2*n+1) |> float_of_int);;

let rec newton_euler n =
  match n with
    | 0 -> (newton 0)
    | n -> (newton n) +. newton_euler (n - 1);;

(*Floating point arithmetic breaks down aroun n =10.*)
newton_euler 9;;

(*Ramanujan-Sato series:
a. Ramanujan’s Series for 1/π: A Survey https://faculty.math.illinois.edu/~berndt/articles/monthly567-587.pdf //
b. RAMANUJAN-LIKE SERIES FOR 1/π2 AND STRING THEORY: https://arxiv.org/pdf/1009.5202.pdf //
c. KIND OF PROOFS OF RAMANUJAN-LIKE SERIES: https://arxiv.org/pdf/1203.1255.pdf //
d. Ramanujan-type formulae for 1/π: A second wind? https://arxiv.org/pdf/0712.1332.pdf
e. Ramanujan, Modular Equations, and Approximations to Pi or How to Compute One Billion Digits of Pi:
https://www.maa.org/sites/default/files/pdf/pubs/amm_supplements/Monthly_Reference_1.pdf*)

let tau = sqrt(8.) /. 99.**2.;;

let phi = fun n -> (factorial (4*n) |> float_of_int)
                  /. (factorial n |> float_of_int)**4.;;

let psi = fun n -> (26390. *. (float_of_int n) +. 1103.)
                   /. 396.**(4. *. float_of_int n);;

let modular_form = fun n -> (phi n) *. (psi n);;

let rec ramanujan n =
   if n < 1 then (modular_form 0)
   else (modular_form n) +. ramanujan (n - 1);;

let ramanujan_pi = fun n -> (1. /. tau) *. (1. /. (ramanujan n));;
