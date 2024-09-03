type monome = int * int * int * int
type polynome = monome array

let reduit (p:polynome) : polynome =
  let n = Array.length p in
  let q = Array.copy p in

  let k = ref 0 in (* prochaine case de q à remplir *)
  for i=0 to n-1 do
    let a, b, c, d = q.(i) in
    if a <> 0 then
      let coef = ref a in
      (* Parcours des monômes p.(i+1)... *)
      (* pour chercher ceux de même degré que p.(i) *)
      for j=i+1 to n-1 do
        let a', b', c', d' = q.(j) in
        if (b, c, d) = (b', c', d') then (
          coef := !coef + a';
          q.(j) <- (0,b,c,d); (* on efface le monôme *)
        )
      done;
      if !coef <> 0 then (
        q.(!k) <- (!coef, b, c, d);
        incr k
      )
    done;
    Array.sub q 0 !k

let reduit_quand_trie (p:polynome) : polynome =
  let n = Array.length p in
  let q = Array.copy p in

  let k = ref 0 in (* prochaine case de q à remplir *)
  let coef = ref 0 in (* coefficient du monôme en cours de calcul *)
  let idx = ref 0 in (* p.(!idx) est le monôme en cours de calcul *)
  for i=0 to n-1 do
    let a, b, c, d = q.(!idx) in (* monôme courant *)
    let a', b', c', d' = q.(i) in
    if a <> 0 then
      let meme_deg = (b, c, d) = (b', c', d') in
      if meme_deg then
        coef := !coef + a'
      else (    (* On a trouvé un monôme de degré différent *)
        if !coef <> 0 then (
          q.(!k) <- (!coef, b, c, d);
          incr k
        );
        (* On met à jour les variables relatives au monôme *)
        idx := i;
        let c, _, _, _ = q.(i) in
        coef := c
      );
      (* On ajoute les monômes du degré restant *)
      if i=n-1 && !coef <> 0 then (
        q.(!k) <- (!coef, b', c', d');
        incr k
      )
  done;
  Array.sub q 0 !k

let meme_degree_permutation (p:monome) (q:monome) : bool =
  let _, b, c, d = p in
  let _, b', c', d' = q in
  (b=b' && c=c' && d=d') || (b=b' && c=d' && d=c')
  || (b=c' && c=b' && d=d') || (b=c' && c=d' && d=b')
  || (b=d' && c=b' && d=c') || (b=d' && c=c' && d=b')

let est_symetrique (p:polynome) : bool =
  let n = Array.length p in
  let p = Array.copy p in
  try
    for i=0 to n-1 do
      let a, b, c, d = p.(i) in
      if a <> 0 then
        let nb_sym  = ref 0 in
        (* Parcours des monômes p.(i+1)... pour chercher ceux *)
        (* de degré égal à p.(i) à une permutation près *)
        for j=i+1 to n-1 do
          let a', b', c', d' = p.(j) in
          if a' <> 0 && meme_degree_permutation p.(i) p.(j) then (
            if a' = a then (
              incr nb_sym;
              p.(j) <- (0, b', c', d')
            ) else raise Exit
          )
        done;
        if !nb_sym <> 5 then raise Exit
    done;
    true
  with Exit -> false

let enumere (k:int) : unit =
  for i=0 to k do
    for j=0 to k-i do
      Printf.printf "X^%d Y^%d Z^%d\n" i j (k-i-j)
    done
  done
