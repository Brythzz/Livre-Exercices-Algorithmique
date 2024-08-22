let coloriage (r:bool array array) : int array =
  let n = Array.length r in
  let couleur = Array.make n 0 in
  let vu = Array.make n false in

  let rec colorier (i:int) (col:int) : unit =
    vu.(i) <- true;
    couleur.(i) <- col;
    for j=0 to n-1 do
      if r.(i).(j) then (* i et j sont voisins *)
        if couleur.(i) = couleur.(j) then
          failwith "coloriage impossible"
        else if not vu.(j) then (
          vu.(j) <- true;
          couleur.(j) <- col;
          let new_col = if col = 1 then 2 else 1 in
          colorier j new_col
        )
    done in

  for i=0 to n-1 do
    if not vu.(i) then
      colorier i 1
  done;
  couleur
