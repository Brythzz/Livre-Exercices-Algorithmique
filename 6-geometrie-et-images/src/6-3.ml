let somme (points:(float * int) array) : (float * float) list =
  let n = Array.length points in
  let intervalles = ref [] in
  let i = ref 0 and j = ref 0 in
  let plusdepos = ref false in
  let plusdeneg = ref false in

  while not (!plusdepos && !plusdeneg) do
    (* positionner i sur le premier élément de poids positif *)
    while !i < n && snd points.(!i) <= 0 do incr i done;
    if !i = n then plusdepos := true;

    (* positionner j sur le premier élément de poids négatif *)
    while !j < n && snd points.(!j) >= 0 do incr j done;
    if !j = n then plusdeneg := true;

    (* étude des cas particuliers *)
    if !plusdepos && !plusdeneg then (* fini *) () else
    if !plusdepos || !plusdeneg then
      failwith "décomposition impossible"
    else ( (* on va retrancher un intervalle à S *)
      let x, xp = points.(!i) in
      let y, yp = points.(!j) in
      points.(!i) <- (x, xp-1);
      points.(!j) <- (y, yp+1);
      intervalles := (x,y)::!intervalles
    )
  done;
  List.rev !intervalles

let incr_valeur_hashtable (ht:(float, int)Hashtbl.t) (k:float) (v:int)
    : unit =
  match Hashtbl.find_opt ht k with
  | Some x -> Hashtbl.replace ht k (x+v)
  | None -> Hashtbl.add ht k v

let rectangles (points:((float * float) * int) array)
    : ((float * float) * (float * float)) list =
  let n = Array.length points in
  let rectangles = ref [] in
  let poids = Hashtbl.create 8 in
  let i = ref 0 and j = ref 0 in
  let plusdepos = ref false in
  let plusdeneg = ref false in

  let y0 = snd (fst points.(n-1)) in
  while not (!plusdepos && !plusdeneg) do
    (* positionner i sur le premier élément de poids positif *)
    while !i < n && snd points.(!i) <= 0 do incr i done;
    if !i = n then plusdepos := true;

    (* positionner j sur le premier élément de poids négatif *)
    while !j < n && snd points.(!j) >= 0 do incr j done;
    if !j = n then plusdeneg := true;

    (* étude des cas particuliers *)
    if !plusdepos && !plusdeneg then (
      (* vérifier que les points de même ordonnée se compensent bien *)
      Hashtbl.iter (fun _ v ->
        if v <> 0 then failwith "décomposition impossible") poids;
      (* fini *)
    ) else if !plusdepos || !plusdeneg then
      failwith "décomposition impossible"
    else ( (* on va retrancher un rectangle à S *)
      let (x1, y1), p1 = points.(!i) in
      let (x2, y2), p2 = points.(!j) in
      if y1 <> y2 then failwith "décomposition impossible";

      points.(!i) <- (x1, y1), p1-1;
      points.(!j) <- (x2, y2), p2+1;
      incr_valeur_hashtable poids x1 1;
      incr_valeur_hashtable poids x2 (-1);
      if y1 <> y0 then rectangles := ((x1,y1), (x2,y0))::!rectangles
    ) (* aux dernières itérations, on décrémente les points sur y0 *)
  done;
  List.rev !rectangles

let _ =
  let points = [|
                ((2.,3.),1);  ((3.,3.),-1);
  ((1.,2.),1);  ((2.,2.),-2); ((3.,2.),1);
  ((1.,1.),-1); ((2.,1.),1) |] in

  rectangles points
