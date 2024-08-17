let nb_possibilites (n:int) : int =
  let nb = ref 0 in
  for nb10 = 0 to (n / 10) do
    let reste10 = n - 10*nb10 in
    for nb5 = 0 to (reste10 / 5) do
      let reste5 = reste10 - 5*nb5 in
      for nb2 = 0 to (reste5 / 2) do
        incr nb
      done
    done
  done;
  !nb

let max1=1 and max2=100 and max5=100 and max10=100

let nb_possibilites_contrainte (n:int) : int =
  let nb = ref 0 in
  for nb10 = 0 to min (n / 10) max10 do
    let reste10 = n - 10*nb10 in
    for nb5 = 0 to min (reste10 / 5) max5 do
      let reste5 = reste10 - 5*nb5 in
      for nb2 = 0 to min (reste5 / 2) max2 do
        let poids_restant = reste5 - 2*nb2 in
        if poids_restant <= max1 then
          incr nb
      done
    done
  done;
  !nb

let nb_camions (n:int) : (int * int * int * int) =
  let n10 = n / 10 in
  let reste = n mod 10 in
  let n5 = if reste >= 5 then 1 else 0 in
  let reste = reste - n5*5 in
  let n2 = reste / 2 in
  let n1 = reste mod 2 in
  (n1, n2, n5, n10)
