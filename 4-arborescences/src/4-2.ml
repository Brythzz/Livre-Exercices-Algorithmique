type couleur = Rouge | Noir
type arn = Nil | Noeud of arn * (int * couleur) * arn

let rec insere_abr (t:arn) (e1:int) : arn = match t with
  | Nil -> Noeud (Nil, (e1, Rouge), Nil)
  | Noeud (fg, (e2, c), fd) ->
    if e1 > e2 then  Noeud (fg, (e2, c), insere_abr fd e1)
    else Noeud (insere_abr fg e1, (e2, c), fd)

let rotate_right (t:arn) : arn = match t with
  | Noeud(Noeud(t1, u, t2), v, t3) -> Noeud(t1, u , Noeud(t2, v, t3))
  | _ -> t

let rotate_left (t:arn) : arn = match t with
  | Noeud(t1, u, Noeud(t2, v, t3)) -> Noeud (Noeud(t1, u, t2), v, t3)
  | _ -> t

let rec rotate_node_left (t:arn) (x:int) : arn = match t with
  | Noeud(t1, (e, c), t2) -> if e = x then rotate_left t
    else if x < e then Noeud(rotate_node_left t1 x, (e, c), t2)
    else Noeud(t1, (e, c), rotate_node_left t2 x)
  | _ -> t
