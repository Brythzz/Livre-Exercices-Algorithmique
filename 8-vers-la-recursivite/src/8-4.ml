let deplacements (n:int) : unit =
  let p = Int.shift_left 1 n in (* 2^n *)
  for i=1 to p-1 do
    let t = ref i and k = ref 0 in
    while !t mod 2 = 0 do
      t := !t / 2;
      incr k
    done;
    let num_disque = !k in
    let deplacement = -2*((n - !k) mod 2) + 1 in
    (* Coup à jouer *)
    let dir = if deplacement = -1 then "gauche" else "droite" in
    Printf.printf "Déplacer le disque %d vers la %s\n" num_disque dir
  done
