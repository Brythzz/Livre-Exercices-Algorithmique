(* un point est un couple d'entiers représentant
  ses coordonnées à l'écran *)
type point = int * int

(* un segment est un couple de points représentant
  ses extrémités *)
type segment = point * point

let trigo (s:segment) (pt:point) : int =
  let (x1, y1), (x2, y2) = s and x, y = pt in
  let det = (x2 - x1) * (y - y1) - (y2 - y1) * (x - x1) in
  if det < 0 then -1
  else if det = 0 then 0
  else 1

let coupe (s1:segment) (s2:segment) : int =
  let x, y = s2 in
  let trig = trigo s1 x in
  if trig = trigo s1 y then
    if trig = 0 then 0 else 1
  else 1

let intersecte (s1:segment) (s2:segment) : bool =
  let (x1, y1), (x2, y2) = s1 in
  let (x1', y1'), (x2', y2') = s2 in
  let intsup1 = coupe s1 s2 in
  let intsup2 = coupe s2 s1 in
  if intsup1 = -1 || intsup2 = -1 then false
  else if intsup1 = 1 || intsup2 = 1 then true
  else if x1 < x1' && x1 < x2' && x2 < x1' && x2 < x2' then false
  else if y1 < y1' && y1 < y2' && y2 < y1' && y2 < y2' then false
  else true

let intersection (s:segment array) : bool =
  let n = Array.length s in
  try
    for i=0 to n-1 do
      for j=i+1 to n-1 do
        if intersecte s.(i) s.(j) then
          raise Exit
      done;
    done;
    false
  with Exit -> true
