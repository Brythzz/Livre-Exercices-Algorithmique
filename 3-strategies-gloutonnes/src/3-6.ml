let organise (deb:int array) (fin:int array) : int list =
  let n = Array.length deb in

  let rec aux i j acc =   (* j = dernier ajouté *)
    if i=n then acc else
    if deb.(i) >= fin.(j) then
      aux (i+1) i (i::acc)
    else
      aux (i+1) j acc
  in List.rev (aux 1 0 [0])
