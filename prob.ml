open Float

(* rounds a float to an integer probabilistically *)
let rand_round n =
  let rem = n -. (floor n) in
  if (Random.float 1.0) > rem
  then to_int (floor n)
  else to_int (ceil n)

(* transform a pair of uniform random numbers to a gaussian random *)
let box_muller u1 u2=
  (sqrt (-2.0 *. (log u1))) *. (cos (2.0*.pi*.u2))


(* generate a random number from a gaussian with mean=mu stddev=sigma *)
let gauss_rand mu sigma =
  (box_muller (Random.float 1.0) (Random.float 1.0))
  *. sigma +. mu
