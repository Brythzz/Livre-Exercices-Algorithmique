type arbre = Noeud of int * int * arbre list

let fusion (a:arbre) (b:arbre) : arbre =
  let Noeud(ordre_a, val_a, fils_a) = a in
  let Noeud(ordre_b, val_b, fils_b) = b in
  if val_a > val_b then Noeud(ordre_b+1, val_b, a::fils_b)
  else Noeud(ordre_a+1, val_a, b::fils_a)

let ordre (a:arbre) : int =
  let Noeud(ordre, _, _) = a in
  ordre

let union_tas (l1:arbre list) (l2:arbre list) : arbre list =
  let rec aux acc l1 l2 = match l1, l2 with
    | a1::q1, a2::q2 -> let Noeud(ordre1, _, _) = a1 in
      let Noeud(ordre2, _, _) = a2 in
      if ordre1 < ordre2 then aux (a1::acc) q1 l2
      else aux (a2::acc) l1 q2
    | x::q, [] | [], x::q -> aux (x::acc) q []
    | _ -> acc
  in List.rev (aux [] l1 l2)

let fusion_tas (t1:arbre list) (t2:arbre list) : arbre list =
  let tas = union_tas t1 t2 in

  (* on fusionne tous les arbres de même ordre *)
  let rec aux acc tas = match tas with
    | a1::a2::q -> if ordre a1 = ordre a2
      then let fus = fusion a1 a2 in
        aux acc (fus::q)
      else
        aux (a1::acc) (a2::q)
    | [x] -> x::acc
    | [] -> acc
  in List.rev (aux [] tas)
