type case = int * int

let nombremax = 256 in

let coups = Array.make_matrix nombremax 64 (0,0) in
let ncases = Array.make nombremax 0 in

(* Calcule la k-ième case atteignable à partir de la case c
  selon l'ordre arbitraire de l'énoncé *)
let case_suivante (c:case) (k:int) : case =
  let i, j = c in
  match k with
    | 1 -> i-2, j+1
    | 2 -> i-1, j+2
    | 3 -> i+1, j+2
    | 4 -> i+2, j+1
    | 5 -> i+2, j-1
    | 6 -> i+1, j-2
    | 7 -> i-1, j-2
    | 8 -> i-2, j-1
    | _ -> failwith "k invalide"

(* Vérifie si la case c a déjà été atteinte, On est à
   l'étape pcour, et on cherche c parmi les cases
   atteintes à un coup i de même parité que pcour,
   pcour compris *)
let existe_deja (c:case) (pcour:int) : bool =
  