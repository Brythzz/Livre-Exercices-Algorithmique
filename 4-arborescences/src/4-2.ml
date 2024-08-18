type arn =
  | Nil
  | R of arn * int * arn
  | N of arn * int * arn

let rec insere_arn (t:arn) (e1:int) : arn = match t with
  | Nil -> R (Nil, e1, Nil)
  | R (fg, e2, fd) ->
    if e1 > e2 then  R (fg, e2, insere_arn fd e1)
    else R (insere_arn fg e1, e2, fd)
  | N (fg, e2, fd) ->
    if e1 > e2 then  R (fg, e2, insere_arn fd e1)
    else R (insere_arn fg e1, e2, fd)

type abr = Nil | Noeud of abr * int * abr

let rotate_right (t:abr) : abr = match t with
  | Noeud(Noeud(t1, u, t2), v, t3) -> Noeud(t1, u , Noeud(t2, v, t3))
  | _ -> t

let rotate_left (t:abr) : abr = match t with
  | Noeud(t1, u, Noeud(t2, v, t3)) -> Noeud (Noeud(t1, u, t2), v, t3)
  | _ -> t

let rec rotate_node_left (t:abr) (x:int) : abr = match t with
  | Noeud(t1, y, t2) -> if y = x then rotate_left t
    else if x < y then Noeud(rotate_node_left t1 x, y, t2)
    else Noeud(t1, y, rotate_node_left t2 x)
  | _ -> t

let corrige_rouge (t:arn) : arn = match t with
  | N (R (R (a, x, b), y, c), z, d)
  | N (R (a, x, R (b, y, c)), z, d)
  | N (a, x, R (R (b, y, c), z, d))
  | N (a, x, R (b, y, R (c, z, d)))
    -> R (N (a, x, b), y, N (c, z, d))
  | t -> t

let rec insere_aux (t:arn) (x:int) : arn =
  match t with
  | Nil -> R (Nil, x, Nil)
  | R (fg, y, fd) ->
    if x = y then t
    else if x > y then corrige_rouge (R(fg, y, insere_aux fd x))
    else corrige_rouge (R(insere_aux fg x, y, fd))
  | N (fg, y, fd) ->
    if x = y then t
    else if x > y then corrige_rouge (N(fg, y, insere_aux fd x))
    else corrige_rouge (N(insere_aux fg x, y, fd))

let cons (t:arn) (fg:arn) (x:int) (fd:arn) : arn = match t with
  | N _ -> N(fg, x, fd) | R _ -> R(fg, x, fd ) | _ -> t
