type point = int * int
type polygone = point array

let clip_bord (axe:int) (p:polygone) : polygone =
  let n = Array.length p in
  (* dans le pire cas, le nouveau polygone a n+1 sommets (convexité) *)
  let clipped = Array.make (n+1) (0,0) in
  let nc = ref 0 in
  let cour = ref 0 in

  let first = ref true in
  while !first || !cour <> 0 do
    first := false;
    let pred = ref !cour in incr cour;
    if !cour = n then cour := 0;
    let x1, y1 = p.(!cour) and x2, y2 = p.(!pred) in
    if (y1 > axe) <> (y2 > axe) then (
      let x = (axe - y1) * (x2 - x1) / (y2 - y1) + x1 in
      clipped.(!nc) <- (x, axe);
      incr nc
    );
    if y1 > axe then (
      clipped.(!nc) <- (x1, y1);
      incr nc
    )
  done;
  Array.sub clipped 0 !nc

let _ =
  let polygone = [|
    (20,0); (10,2); (0,10); (4,20); (17,30); (31,16)
  |] in
  clip_bord 6 polygone
  (* résultat : https://www.desmos.com/calculator/hcirr4fhek *)

let clip_bord (t:int) (axe:int) (p:polygone) : polygone =
  let n = Array.length p in
  let clipped = Array.make (n+1) (0,0) in
  let nc = ref 0 in
  let cour = ref 0 in

  let sg = if t mod 2 = 0 then 1 else -1 in
  let first = ref true in
  while !first || !cour <> 0 do
    first := false;
    let pred = ref !cour in incr cour;
    if !cour = n then cour := 0;

    let (x1, y1), (x2, y2) = if t < 2 then p.(!cour), p.(!pred)
      else let (y1, x1), (y2, x2) = p.(!cour), p.(!pred)
      in (x1, y1), (x2, y2) in (* on inverse x et y *)

    if (sg * y1 > sg * axe) <> (sg * y2 > sg * axe) then (
      let x = (axe - y1) * (x2 - x1) / (y2 - y1) + x1 in
      clipped.(!nc) <- if t < 2 then (x, axe) else (axe, x);
      incr nc
    );
    if sg * y1 > sg * axe then (
      clipped.(!nc) <- if t < 2 then (x1, y1) else (y1, x1);
      incr nc
    )
  done;
  Array.sub clipped 0 !nc

let _ =
  let polygone = [|
    (20,0); (10,2); (0,10); (4,20); (17,30); (31,16)
  |] in
  clip_bord 3 15 polygone
  (* résultats : https://www.desmos.com/calculator/5fze7kb8i2 *)

type fenetre = point * point
let clipping (w:fenetre) (p:polygone) =
  let (x1, y1), (x2, y2) = w in
  let p1 = clip_bord 0 y1 p in
  let p2 = clip_bord 1 y2 p1 in
  let p3 = clip_bord 2 x1 p2 in
  clip_bord 3 x2 p3

let _ =
  let polygone = [|
    (20,0); (10,2); (0,10); (4,20); (17,30); (31,16)
  |] in
  let fenetre = (7,5), (25,31) in
  clipping fenetre polygone
  (* résultat : https://www.desmos.com/calculator/rvcw8hm46e *)

let _ =
  let polygone2 = [|
    (10,0); (0,15); (41,15); (17,7); (52,0); (10,0)
  |] in
  clip_bord 2 26 polygone2
  (* résultat obtenu en modifiant la ligne 38 car un polygone
  non convexe peut avoir un nombre arbitraire de nouveaux sommets :
  https://www.desmos.com/calculator/zr2allu0ay *)
