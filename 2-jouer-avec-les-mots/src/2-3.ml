let longueur_sous_suite (a:int array) : int =
  let n = Array.length a in
  let l = Array.make n 0 in
  l.(0) <- 1;
  for p=1 to n-1 do
    let max = ref 0 in
    for j=0 to p-1 do
      if (a.(j) <= a.(p)) && (l.(j)+1 >= !max)
        then max := l.(j) + 1
    done;
    l.(p) <- !max
  done;
  (* Trouver le max *)
  let longueur = ref 1 in
  for i=1 to n-1 do
    if l.(i) > !longueur then
      longueur := l.(i)
  done;
  !longueur

let afficher_sous_suite (a:int array) : unit =
  let n = Array.length a in
  let l = Array.make n 0 in
  let precedent = Array.make n 0 in
  l.(0) <- 1;
  for p=1 to n-1 do
    let max = ref 0 and prec = ref 0 in
    for j=0 to p-1 do
      if (a.(j) <= a.(p)) && (l.(j)+1 >= !max)
        then (
          max := l.(j) + 1;
          prec := j
        )
    done;
    l.(p) <- !max; precedent.(p) <- !prec
  done;
  (* Trouver le max *)
  let longueur = ref 1 and pmax = ref 0 in
  for i=1 to n-1 do
    if l.(i) > !longueur then (
      longueur := l.(i);
      pmax := i
    )
  done;
  Printf.printf "%d" a.(!pmax);
  for i=1 to !longueur-1 do
    pmax := precedent.(!pmax);
    Printf.printf " %d" a.(!pmax);
  done
