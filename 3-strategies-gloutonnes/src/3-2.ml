let hauteurs (p:int array) : (int array * int) =
  let n = Array.length p in
  let hauteurs = Array.make n 1 in
  let adroite = Array.make n (-1) in
  adroite.(0) <- 0;
  let hmax = ref 1 in

for i=1 to n-1 do
  (* calculer la hauteur de pi *)
  let h = ref !hmax in
  while !h > 0 && p.(i) < p.(adroite.(!h-1)) do
    decr h
  done;
  hauteurs.(i) <- !h+1;
  adroite.(!h) <- i;

  if !h+1 > !hmax then
    hmax := !h+1
  done;
  hauteurs, !hmax
