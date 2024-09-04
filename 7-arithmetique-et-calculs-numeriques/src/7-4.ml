type nombre = int array
let base = 128 (* 2^7 *)

let addition (a:nombre) (b:nombre) : nombre =
  let n = Array.length a in
  let s = Array.make n 0 in

  let retenue = ref 0 in
  for i=0 to n-1 do
    s.(i) <- a.(i) + b.(i) + !retenue;
    retenue := s.(i) / base;
    s.(i) <- s.(i) mod base
  done;
  if !retenue <> 0 then
    failwith "dépassement d'entier";
  s

let multiplication_chiffre (a:nombre) (c:int) : nombre =
  let n = Array.length a in
  let p = Array.make n 0 in

  let retenue = ref 0 in
  for i=0 to n-1 do
    p.(i) <- c * a.(i) + !retenue;
    retenue := p.(i) / base;
    p.(i) <- p.(i) mod base
  done;
  if !retenue <> 0 then
    failwith "dépassement d'entier";
  p

let n = 5
let m = [|11; 123; 0; 0; 0|]
let m' = 93
let r_carre = [|84; 85; 0; 0; 0|]

let multiplication_montg (a:nombre) (b:nombre) : nombre =
  let n = Array.length a in
  let s = ref [||] in

  for i=0 to n-1 do
    let t = multiplication_chiffre b a.(i) in
    s := addition !s t;
    let q = (!s.(0) * m') mod base in
    let t = multiplication_chiffre m q in
    s := addition !s t;
    (* division par r (décalage) *)
    for j=0 to n-2 do
      !s.(j) <- !s.(j+1)
    done;
    !s.(n-1) <- 0
  done;
  !s

let multiplication (a:nombre) (b:nombre) : nombre =
  let c = multiplication_montg a b in
  let c = multiplication_montg c r_carre in

  (* On regarde si c < m *)
  try
    for i=n-1 downto 0 do
      if c.(i) < m.(i) then
        raise Exit
    done;
    let m' = multiplication_chiffre m (-1) in
    addition c m'
  with Exit -> c
