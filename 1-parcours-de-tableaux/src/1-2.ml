let echange (a:int array) (i:int) (j:int) : unit =
  let tmp = a.(i) in
  a.(i) <- a.(j);
  a.(j) <- tmp

let partition (a:int array) (g:int) (d:int) : int =
  let alpha = a.(g) in
  let i = ref g and j = ref d in
  while !i < !j do (
    while a.(!j) >= alpha && !j > g do
      decr j
    done;
    while a.(!i) <= alpha && !i < d do
      incr i;
    done;
    if (!i < !j)
      then echange a !i !j
  ) done;
  echange a !j g;
  !j (* on renvoie le pivot *)

let mediane (a:int array) : int =
  let n = Array.length a in
  let g, d = ref 0, ref (n-1) in
  while !d - !g > 1 do
    let pivot = partition a !g !d in
    if pivot < n/2
      then g := pivot+1
      else d := pivot-1
  done;
  !g
