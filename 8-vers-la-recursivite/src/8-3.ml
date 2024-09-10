type point = int * int

let dist_sq (p1:point) (p2:point) : int =
  let x1, y1 = p1 and x2, y2 = p2 in
  (x2-x1)*(x2-x1) + (y2-y1)*(y2-y1)
let couple (tab:point array) : (point * point) =
  let n = Array.length tab in
  let min_dist = ref max_int in
  let p1 = ref (0,0) and p2 = ref (0,0) in

  for i=0 to n-2 do
    for j=i+1 to n-1 do
      let dist = dist_sq tab.(i) tab.(j) in
      if dist < !min_dist then (
        min_dist := dist;
        p1 := tab.(i);
        p2 := tab.(j)
      )
    done
  done;
  (!p1, !p2)

let decoupe (x:point array) (y:point array) (y':point array)
    (deb:int) (fin:int) : unit =
  let med = (deb + fin) / 2 in
  let i = ref deb in
  let j = ref (med + 1) in
  for k=deb to fin do
    if fst y.(k) > fst x.(med) then (
      y'.(!j) <- y.(k);
      incr j
    )
    else (
      y'.(!i) <- y.(k);
      incr i
    )
  done

let termine (x:point array) (y:point array) (deb:int) (fin:int)
    (p1:point) (p2:point) : (point * point) * int =
  let dist = ref (dist_sq p1 p2) in
  let n = Array.length x in
  let z = Array.make n (0,0) in
  let t = ref 0 in

  let med = (deb + fin) / 2 in
  let a = fst x.(med) - !dist in
  let b = fst x.(med) + !dist in
  (* On ajoute tous les candidats potentiels *)
  for k=deb to fin do
    if a < fst y.(k) && fst y.(k) < b then (
      z.(!t) <- y.(k);
      incr t
    )
  done;
  (* On cherche une plus petite distance *)
  let p1 = ref p1 and p2 = ref p2 in
  for k=0 to !t-2 do
    let i = ref 1 in
    while !i + k < !t && (snd z.(k + !i)) - (snd z.(k)) < !dist do
      let d = dist_sq z.(k) z.(k + !i) in
      if d < !dist then (
        p1 := z.(k);
        p2 := z.(k+1);
        dist := d
      );
      incr i
    done
  done;
  (!p1, !p2), !dist


let cherche (x:point array) (y:point array) : point * point =
  let n = Array.length x in
  let y' = Array.copy y in

  let rec cherche_aux deb fin =
    if fin = deb then
      ((0,0), (0,0)), max_int
    else if fin = deb + 1 then
      (x.(deb), x.(fin)), dist_sq x.(deb) x.(fin)
    else (    (* fin > deb+1 *)
      let med = (deb+fin)/2 in
      decoupe x y y' deb fin;
      let (a, b), d1 = cherche_aux deb med in
      let (c, d), d2 = cherche_aux (med+1) fin in
      let p1, p2 = if d1 < d2 then  a, b else c, d in
      termine x y deb fin p1 p2
    )
  in let (p1, p2), d = cherche_aux 0 (n-1) in
  p1, p2

let _ =
  let x = [|(100, 430); (140, 300); (210, 120); (215, 240); (225, 400); (300, 330); (350, 160); (400, 229); (425, 425); (475, 370); (509, 140); (590, 290); (625, 240); (680, 170)|] in
  let x = [|(100,430); (140,300); (210,120); (215,240); (225,400); (330,425); (350,160); (395,230); (360,425); (475,370); (510,140); (590,290); (625,240); (680,170) |] in
  let y = Array.copy x in
  Array.sort (fun (_, ay) (_, by) -> if ay = by then 0 else if ay > by then 1 else -1) y;
  cherche x y
