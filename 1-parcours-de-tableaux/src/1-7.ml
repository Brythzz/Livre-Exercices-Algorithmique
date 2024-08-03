let est_permutation (p:int array) : bool =
  let n = Array.length p in
  let vu = Array.make n false in
  Array.iter (fun e -> if 0 <= e && e < n then vu.(e) <- true) p;
  Array.fold_left (&&) true vu

let afficher_cycles (p:int array) : unit =
  let n = Array.length p in
  let vu = Array.make n false in

  for i=0 to n-1 do (* recherche du premier élément non vu *)
    if not vu.(i) then (
      vu.(i) <- true;
      Printf.printf "(%d" i;
      let j = ref p.(i) in
      while !j <> i do (* parcours du cycle *)
        vu.(!j) <- true;
        Printf.printf " %d" !j;
        j := p.(!j)
      done;
      print_endline ")"
    )
  done

let max_croit (p:int array) : int =
  let n = Array.length p in
  let i = ref (n-2) in
  while !i >= 0 && p.(!i) >= p.(!i+1) do
    decr i;
  done;
  !i

let inf (p:int array) (k:int) : int =
  let n = Array.length p in
  let min = ref max_int in
  for i=k+1 to n-1 do
    if p.(i) < !min && p.(i) > p.(k) then
      min := p.(i)
  done;
  !min

let suivante (p:int array) : (int array) =
  assert (est_permutation p);

  let n = Array.length p in
  let q = Array.make n 0 in
  let seuil = max_croit p in
  if (seuil < 0) then [||] (* pas de permutation suivante *)
  else

  (* recopier les éléments avant le seuil *)
  (* en les marquant comme vus *)
  let vu = Array.make n false in
  for i=0 to seuil-1 do
    q.(i) <- p.(i);
    vu.(p.(i)) <- true
  done;

  let inf = inf p seuil in
  q.(seuil) <- inf;
  vu.(inf) <- true;

  (* complèter q avec les éléments non vus *)
  let idx = ref (seuil+1) in
  for i=0 to n-1 do
    if not vu.(i) then (
      q.(!idx) <- i;
      vu.(i) <- true;
      incr idx
    )
  done;
  q

let affiche_perm (p:int array) : unit =
  let n = Array.length p in
  print_string "(";
  Array.iteri (fun i e -> if i = n-1
    then Printf.printf "%d)\n" e
    else Printf.printf "%d " e
  ) p

let enumere (n:int) : unit =
  (* Initialisation de la permutation identité *)
  let identite = Array.init n (fun i -> i) in
  let cur = ref identite in

  while !cur <> [||] do
    affiche_perm !cur;
    cur := suivante !cur
  done
