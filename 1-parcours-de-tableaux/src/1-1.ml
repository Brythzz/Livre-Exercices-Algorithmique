type case = int * int

let nombremax = 256

let coups = Array.make_matrix nombremax 64 (0,0)
let ncases = Array.make nombremax 0

(* Retourne la k-ième case atteignable a partir de
  la case c selon l'ordre arbitraire de l'énoncé *)
let case_suivante (c:case) (k:int) : case =
  assert (1 <= k && k <= 8);
  let i, j = c in
  let positions = [|
    (i-2, j+1); (i-1, j+2); (i+1, j+2); (i+2, j+1);
    (i+2, j-1); (i+1, j-2); (i-1, j-2); (i-2, j-1)
  |] in
  positions.(k-1)

(* Vérifie si la case c a déjà été atteinte, On est à
   l'étape pcour, et on cherche c parmi les cases
   atteintes à un coup i de même parité que pcour,
   pcour compris *)
let existe_deja (c:case) (pcour:int) : bool =
  