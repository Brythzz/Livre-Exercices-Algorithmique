let rapide (d:int array) (r:int) : int list =
  let n = Array.length d in
  let rec aux acc i reste =
    if i=n then acc else
    if d.(i) > reste then
      aux (i::acc) (i+1) (r-d.(i))
    else
      aux acc (i+1) (reste-d.(i))
  in List.rev (aux [] 0 r)

