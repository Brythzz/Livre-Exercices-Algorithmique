type couleur = Blanc | Gris | Noir

let est_acyclique (g:bool array array) : bool =
  let n = Array.length g in
  let couleur = Array.make n Blanc in

  let rec parcours i =
    for j=0 to n-1 do
      if g.(i).(j) && couleur.(j) <> Noir then (
        if couleur.(j) = Gris then raise Exit;

        couleur.(j) <- Gris;
        parcours j;
        couleur.(j) <- Noir
      )
    done in

  try
    for i=0 to n-1 do
      if couleur.(i) <> Noir then
        parcours i
    done;
    true
  with Exit -> false

let est_graphe_dep (g:bool array array) (d:bool array array)
    (etiq:int array) : bool =
  let n = Array.length g in
  try
    for i=0 to n-1 do
      for j=0 to n-1 do
        if d.(etiq.(i)) = d.(etiq.(j)) then
          if not (i=j || g.(i).(j) || g.(j).(i)) then
            raise Exit;
        if (i=j || g.(i).(j) || g.(j).(i)) then
          if not (d.(etiq.(i)) = d.(etiq.(j))) then
            raise Exit
        done
      done;

      est_acyclique g
  with Exit -> false

(* Retourne un noeud non vu et sans prédécesseur *)
let noeud_sans_pred (g:bool array array) (vus:bool array) : int =
  let n = Array.length g in
  let res = ref 0 in
  try
    for i=0 to n-1 do
      if not vus.(i) then
        let a_pred = ref false in
        for j=0 to n-1 do
          a_pred := !a_pred || g.(j).(i)
        done;
        if not !a_pred then (
          res := i;
          raise Exit
        )
    done; 0 (* impossible *)
  with Exit -> !res

let effacer_aretes_sortantes (g:bool array array)
    (x:int) : unit =
  let n = Array.length g in
  for i=0 to n-1 do
    g.(x).(i) <- false
  done

let lineariser (g:bool array array) (etiq:int array) : int array =
  let n = Array.length g in
  let suite = Array.make n 0 in
  let vus = Array.make n false in

  for i=0 to n-1 do
    let x = noeud_sans_pred g vus in
    effacer_aretes_sortantes g x;
    suite.(i) <- etiq.(x);
    vus.(x) <- true
  done;
  suite
