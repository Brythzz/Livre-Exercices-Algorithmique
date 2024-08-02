let deuxieme (t:int array) : int =
  let n = Array.length t in
  let max_1 = ref (max t.(0) t.(1)) in
  let max_2 = ref (min t.(0) t.(1)) in
  for i=2 to n-1 do
    if t.(i) > !max_1 then
      (max_2 := !max_1; max_1 := t.(i))
    else if t.(i) > !max_2 then
      max_2 := t.(i)
  done;
  !max_2

type tree = Node of (tree * int * tree) | Leaf of int

let etiquette tree = match tree with
  | Node(_, x, _) | Leaf x -> x

let deuxieme (t:int array) : int =
  let n = Array.length t in
  (* liste de tous les joueurs *)
  let joueurs = List.init n (fun i -> Leaf i) in

  (* simulation des duels, retourne un arbre de tournoi *)
  let rec jouer jou gagn = match jou with
    | j1::j2::q ->
      let i1, i2 = etiquette j1, etiquette j2 in
      let g = if t.(i1) > t.(i2) then i1 else i2 in
      let res = Node (j1, g, j2) in
      jouer q (res::gagn)
    | [g] when gagn = [] -> g
    | l -> jouer (l@gagn) [] (* un seul joueur ou liste vide *)
  in
  let matchs = jouer joueurs [] in

  (* on descend dans l'arbre pour trouver *)
  (* le plus grand adversaire du premier *)
  let premier = etiquette matchs in
  let rec traverser res m = match res with
    | Leaf _ -> m
    | Node (g, gagn, d) ->
      if etiquette g = premier then
        let idx = etiquette d in
        let m = max m t.(idx) in
        traverser g m
      else
        let idx = etiquette g in
        let m = max m t.(idx) in
        traverser d m
  in traverser matchs min_int
