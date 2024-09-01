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

type arbre =
  | Nil
  | R of arbre * segment * arbre
  | N of arbre * segment * arbre
let insere_abr (t:arbre) (s:segment) : arbre = Nil
let supprime_abr (t:arbre) (s:segment) : arbre = Nil
let en_dessous (t:arbre) (s:segment) : segment option = None
let au_dessus (t:arbre) (s:segment) : segment option = None

type tas = (point * segment) array
let init_tas (n:int) : tas = [|(0,0), ((0,0), (0,0))|]
let insere_tas (t:tas) (e:point * segment) = ()
let retire_tas (t:tas) = (0,0), ((0,0), (0,0))

let test_intersection (t:arbre) (s:segment) : unit =
  match au_dessus t s with
  | Some x when intersecte x s -> raise Exit
  | _ -> ();
match en_dessous t s with
  | Some x when intersecte x s -> raise Exit
  | _ -> ()

let intersection (s:segment array) : bool =
  let n = Array.length s in
  let tas = init_tas (2*n) in
  let arbre = ref Nil in

  for i=0 to n-1 do
    let p1, p2 = s.(i) in
    insere_tas tas (p1, s.(i));
    insere_tas tas (p2, s.(i))
  done;

  try
    for i=0 to 2*n-1 do
      let p, s = retire_tas tas in
      let p1, p2 = s in
      if p = p1 then (    (* Cas 1 *)
        arbre := insere_abr !arbre s;
        test_intersection !arbre s
      ) else (    (* Cas 2 *)
        test_intersection !arbre s;
        arbre := supprime_abr !arbre s
      )
    done;
    false
  with Exit -> true
