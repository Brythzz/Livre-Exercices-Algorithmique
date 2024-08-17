let poids_max (objets:int list) (poids_max:int) : int =
  let rec aux objets poids = match objets with
    | x::q -> if poids + x <= poids_max
      then max (aux q (poids + x)) (aux q poids)
      else aux q poids
    | [] -> poids
  in aux objets 0
