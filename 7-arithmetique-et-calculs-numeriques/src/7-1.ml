let numero (p:int array) : int =
  let n = Array.length p in
  let s = ref 0 in
  let pow = ref 1 in
  for i=0 to n-1 do
    s := !s + p.(i) * !pow;
    pow := 2 * !pow
  done;
  !s

let partie (k:int) (n:int) : int array =
  let p = Array.make n 0 in

  let k = ref k in
  for i = 0 to n-1 do
    p.(i) <- !k mod 2;
    k := !k/2;
  done;
  if !k <> 0 then failwith "k est trop grand";
  p

let partie2 (k:int) (n:int) : int array =
  let p = Array.make n 0 in
  let rec aux k i =
    if i > n then failwith "k est trop grand";
    if k > 0 then (
      p.(i) <- k mod 2;
      aux (k/2) (i+1)
    )
  in aux k 0;
  p

let plus_un (p:int array) : int array =
  let n = Array.length p in
  let q = Array.copy p in

  let rec aux i =
    if i > 0 then (
      if p.(i) = 1 then (
        q.(i) <- 0;
        aux (i-1)
      ) else q.(i) <- 1
    ) else if p.(i) = 1 then
      failwith "pas de successeur"
    else q.(i) <- 1
  in aux (n-1);
  q

let affiche_partie (p:int array) =
  let n = Array.length p in
  Printf.printf "{";
  let first = ref true in
  for i=0 to n-1 do
    if p.(i) = 1 && !first then
      (Printf.printf "%d" i; first := false)
    else if p.(i) = 1 then
      Printf.printf ", %d" i
  done; Printf.printf "}\n"
let enumere (n:int) : unit =
  (* initialisation de la partie vide *)
  let p = ref (Array.make n 0) in
  try
    while true do
      affiche_partie !p;
      p := plus_un !p
    done;
  with _ -> ()
