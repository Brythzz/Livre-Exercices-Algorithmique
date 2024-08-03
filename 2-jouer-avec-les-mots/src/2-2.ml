let rec motdepliage (n:int) : (int list) =
  if n = 0 then [0] else
  let pred = motdepliage (n-1) in
  let pred' = List.map (fun e -> 1-e) (List.rev pred) in
  pred @ (0::pred')

let binaire (n:int) : (int list) =
  let rec aux n acc =
    if n = 0 then acc
    else aux (n / 2) ((n mod 2) :: acc)
  in
  if n = 0 then [0] else aux n []

let bit (n:int) : int =
  assert(n > 0);
  let n = ref n in
  while !n mod 2 = 0 do
    n := Int.shift_right !n 1
  done;
  if !n mod 4 = 1 then 0 else 1
