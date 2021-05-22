(*Here we consider the exponential of the Meissel-Mertens constant: https://oeis.org/wiki/Meissel%E2%80%93Mertens_constant*)

(*A prime sieve over odd numbers.*)
let prime_sieve n =
  let m = (n / 2) + 1 in
  let primes = Array.init m (fun i->2*i+1) in
  primes.(0) <- 2;

  let last_integer = int_of_float (sqrt (float_of_int n) /. 2.) + 1 in

  let idx = ref 1 in
  while !idx <= last_integer do
    if primes.(!idx) <> 0 then

      (*Remove multiples*)
      (let p = primes.(!idx) in
       let j = ref (!idx * (p + 1)) in
       while !j < m do
         primes.(!j) <- 0;
         j := !j + p;
       done);
    idx := !idx + 1;
  done;

List.filter ((<>) 0) (Array.to_list primes);;

let prime_entropy primes =

    let sum l = List.fold_left (+.) 0. l in
    let f x = 1. /. (float_of_int x) in

    let entropy = primes |> (List.map f) |> sum in

entropy
;;

let exp_mm n =
  let primes = prime_sieve n in
  let exp_entropy = primes |> prime_entropy |> exp in

exp_entropy /. (n |> float_of_int |> log)
;;
