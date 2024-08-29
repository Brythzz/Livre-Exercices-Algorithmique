type point = int * int
type polygone = point array

let saillant (a:point) (b:point) (c:point) : bool =
  let ax, ay = a and bx, by = b and cx, cy = c in
  let det = (ax - bx) * (cy - by) - (ay - by) * (cx - bx) in
  det > 0

let tester (p:polygone) : bool =
  let n = Array.length p in
  let test = saillant p.(0) p.(1) p.(2) in

  try
    for i=1 to n-1 do
      let cour = saillant p.(i) p.((i+1) mod n) p.((i+2) mod n) in
      if cour <> test then
        raise Exit
    done;
    true
  with Exit -> false

let convexe (p:polygone) : bool =
  let n = Array.length p in
  let test = saillant p.(0) p.(1) p.(2) in
  let croit = ref (fst p.(0) < fst p.(1)) in
  let sens = ref 0 in

  try
    for i=1 to n-1 do
      let cour = saillant p.(i) p.((i+1) mod n) p.((i+2) mod n) in
      if !croit <> (fst p.(i) <= fst p.((i+1) mod n)) then (
        incr sens;
        croit := not !croit
      );
      if !sens >= 3 || cour <> test then
        raise Exit
    done;
    true
  with Exit -> false

let enveloppe (e:point array) : polygone =
  let n = Array.length e in
  let env = Array.make n (0,0) in

  let min = ref 0 in
  Array.iteri (fun i _ -> if snd e.(i) < snd e.(!min) then min := i) e;

  let suiv = ref !min in
  let np = ref 0 in
  while !suiv <> !min || !np = 0 do
    let cour = !suiv in
    env.(!np) <- e.(cour);
    incr np;
    suiv := (cour + 1) mod n;

    for i=2 to n-1 do
      if not (saillant e.(!suiv) e.(cour) e.((cour + i) mod n)) then
        suiv := (cour + i) mod n
    done
  done;
  Array.sub env 0 !np

let balayage (e:point array) : polygone =
  let n = Array.length e in
  let p = Array.make n (0,0) in
  let np = ref 0 in

  for i=0 to 2 do
    p.(!np) <- e.(i);
    incr np
  done;
  for i=3 to n-1 do
    while not (saillant e.(!np-2) e.(!np-1) e.(i)) do decr np done;
    p.(!np) <- e.(i);
    incr np
  done;
  p

let _ =
  let polygone_schema = [|
    (* 1      2        3       4       5       6 *)
    (5,28); (6,11); (30,16); (0,20); (6,3); (42,17)
  |] in

  let c1 = tester polygone_schema in
  assert (c1 = true);

  let polygone_non_convexe = [|
    (0,0); (0,2); (1,1); (2,2); (2,0);
  |] in

  let c2 = tester polygone_non_convexe in
  assert(c2 = false);

  (* enveloppe polygone_schema *)

  let polygone_schema_ord = [|
    (*  3        6       4         1      2       5 *)
     (30,16); (42,17); (0,20);  (5,28); (6,11); (6,3);
  |] in
  balayage polygone_schema_ord
