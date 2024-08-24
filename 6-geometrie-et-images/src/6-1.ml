let largeur2 (n:int) : int =
  if n=1 then 1 else
  if n=2 then 2 else

  let a = ref 1 and b = ref 2 in
  for i=3 to n do
    (* Calcul de u_i *)
    let tmp = !a + !b in
    a := !b;
    b := tmp
  done;
  !b

let rec puiss (x:int) (n:int) : int = match n with
  | 0 -> 1
  | n when (n mod 2) = 0 -> puiss (x * x) (n / 2)
  | n -> (puiss (x * x) ((n-1)/2)) * x

let largeur3 (n:int) : int =
  if n mod 2 = 1 then 0 else
  let wn = ref 1 in (* valeur de w1 *)
  let un = ref 2 in (* valeur de u2 *)
  for i=2 to (n/2) do
    (* Calcul de w2i-1 et de u2i *)
    wn := !wn + !un;
    un := 2 * !wn + !un
  done;
  !un
