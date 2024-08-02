type case = int * int

let nombre_max_coups = 256 (* nombre max de coups *)
let coups = Array.make_matrix nombre_max_coups 64 (0,0)
let ncases = Array.make nombre_max_coups 0

let est_valide (c:case) : bool =
  (0 <= fst c) && (snd c <= 7)

(* Retourne la k-ième case atteignable a partir de *)
(* la case c selon l'ordre arbitraire de l'énoncé *)
let case_suivante (c:case) (k:int) : case =
  assert (1 <= k && k <= 8);
  let i, j = c in
  let positions = [|
    (i-2, j+1); (i-1, j+2); (i+1, j+2); (i+2, j+1);
    (i+2, j-1); (i+1, j-2); (i-1, j-2); (i-2, j-1)
  |] in
  positions.(k-1)

(* Vérifie si la case c a déjà été atteinte. On est à *)
(* l'étape pcour, et on cherche c parmi les cases *)
(* atteintes à un coup i de même parité que pcour, *)
(* pcour compris *)
let existe_deja (c:case) (pcour:int) : bool =
  let i = ref (pcour mod 2) in
  try
    while !i <= pcour do
      for j=0 to ncases.(!i)-1 do
        if coups.(!i).(j) = c then raise Exit
      done;
      i := !i + 2
    done;
    false
  with Exit -> true

let accessibles (i0:int) (j0:int) (p:int) : unit =
  assert (p < nombre_max_coups);
  coups.(0).(0) <- (i0, j0);
  ncases.(0) <- 1;

  for p_courant=1 to p do
    (* on ajoute les cases atteignables en p coups *)
    (* i.e en 1 coup depuis les cases atteignables en p-1 coups *)
    for idx = 0 to ncases.(p_courant-1)-1 do
      let case_courante = coups.(p_courant-1).(idx) in
      for k=1 to 8 do
        let cs = case_suivante case_courante k in
        if est_valide cs && not (existe_deja cs p_courant) then (
          (* On insère la nouvelle case *)
          let nb_cases = ncases.(p_courant) in
          coups.(p_courant).(nb_cases) <- cs;
          ncases.(p_courant) <- nb_cases + 1;
        )
      done
    done
  done
