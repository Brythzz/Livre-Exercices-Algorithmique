let verif (t:int array) : bool =
  let n = t.(0) in
  try
    for i=1 to n/2 do
      if t.(i) > t.(2*i) then raise Exit;
      if 2*i+1 <= n && t.(i) > t.(2*i+1) then raise Exit;
    done;
    true
  with Exit -> false

let fils_min (t:int array) (i:int) : int =
  let n = t.(0) in
  let a_fils_droit = 2*i+1 <= n in
  if a_fils_droit then
    if t.(2*i) < t.(2*i+1) then
      2*i else 2*i+1
  else 2*i
let rec percolation_basse (t:int array) (i:int) : unit =
  let n = t.(0) in
  let a_fils_gauche = 2*i <= n in
  if a_fils_gauche then
    let fils = fils_min t i in
    if t.(fils) < t.(i) then (
      let tmp = t.(i) in
      t.(i) <- t.(fils);
      t.(fils) <- tmp;
      percolation_basse t fils
    )

let supprimer_racine (t:int array) : int =
  let n = t.(0) and rac = t.(1) in
  t.(1) <- t.(n);
  t.(0) <- n-1;
  percolation_basse t 1;
  rac

let rec percolation_haute (t:int array) (i:int) : unit =
  let parent = i/2 in
  if i <> 1 && t.(parent) > t.(i) then (
    let tmp = t.(i) in
    t.(i) <- t.(parent);
    t.(parent) <- tmp;
    percolation_haute t parent
  )

let ajouter (t:int array) (e:int) : unit =
  assert(Array.length t > t.(0) + 1);
  t.(0) <- t.(0) + 1;
  let n = t.(0) in
  t.(n) <- e;
  percolation_haute t n

let tri_par_tas (a:int array) : unit =
  let n = Array.length a in
  let tas = Array.make (n+1) 0 in
  Array.iter (ajouter tas) a;
  for i=0 to n-1 do
    a.(i) <- supprimer_racine tas
  done
