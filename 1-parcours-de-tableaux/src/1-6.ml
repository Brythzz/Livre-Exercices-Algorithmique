let nb_points_fixes (p:int array) : int =
  let pts = ref 0 in
  Array.iteri (fun i e -> if i = e then incr pts) p;
  !pts

let nb_cycles (p:int array) : int =
  let n = Array.length p in
  let vu = Array.make n false in
  let nb_cycles = ref 0 in

  for i=0 to n-1 do
    if not vu.(i) then ( (* nouveau cycle *)
      incr nb_cycles;
      vu.(i) <- true;
      let j = ref p.(i) in
      while !j <> i do (* parcours du cycle *)
        vu.(!j) <- true;
        j := p.(!j)
      done;
    )
  done;
  !nb_cycles

let transforme (p:int array) : int array =
  let n = Array.length p in
  let nb_cycles = ref 0 in
  let min = Array.make n 0 in
  let vu = Array.make n false in

  for i=0 to n-1 do
    if not vu.(i) then ( (* nouveau cycle *)
      min.(!nb_cycles) <- i;
      incr nb_cycles;
      vu.(i) <- true;
      let j = ref p.(i) in
      while !j <> i do (* parcours du cycle *)
        vu.(!j) <- true;
        j := p.(!j)
      done;
    )
  done;

  let sortie = Array.make n 0 in
  let k = ref 0 in (* première case libre du tableau sortie *)
  for j=0 to !nb_cycles-1 do (* écrire les cycles successivement *)
    (* recherche du max *)
    let idx_max = ref 0 in
    for i=1 to !nb_cycles-1 do
      if min.(i) > min.(!idx_max) then
        idx_max := i
    done;

    let max = min.(!idx_max) in
    min.(!idx_max) <- 0;
    sortie.(!k) <- max;
    incr k;
    let j = ref p.(max) in
    while !j <> max do (* parcours du cycle *)
      sortie.(!k) <- !j;
      incr k;
      j := p.(!j)
    done;
  done;
  sortie

let inverse (c:int array) : int array =
  let n = Array.length c in
  let p = Array.make n 0 in
  let min = ref c.(0) in
  for i=0 to n-2 do (* trouver l'image de c(i) *)
    if c.(i+1) > !min
      then p.(c.(i)) <- c.(i+1)
      else (
        p.(c.(i)) <- !min;
        min := c.(i+1)
      )
  done;
  p.(c.(n-1)) <- !min; p
