let est_carre (facteur:string) : bool =
  let n = String.length facteur in
  if n mod 2 <> 0 then false else
  let p = n/2 in
  try
    for i=0 to p-1 do
      if facteur.[i] <> facteur.[p+i] then
        raise Exit
    done;
    true
  with Exit -> false

let contient_carre (mot:string) : bool =
  let n = String.length mot in
  try
    for i=0 to n-1 do
      for j=i+1 to n do
        let fac = String.sub mot i (j-i) in
        if est_carre fac then
          raise Exit
      done
    done;
    false
  with Exit -> true

(* génère les 2n premiers chiffres de la suite *)
let thue_morse (n:int) : int array =
  let mot = Array.make (2*n) 0 in
  for i=0 to n-1 do
    if mot.(i) = 0 then (
      mot.(2*i) <- 0;
      mot.(2*i + 1) <- 1
    ) else (
      mot.(2*i) <- 1;
      mot.(2*i + 1) <- 0
    )
  done;
  mot

