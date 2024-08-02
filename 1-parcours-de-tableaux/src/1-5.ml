let ranger (t:int array) (i:int) (j:int) : unit =
  if t.(i) > t.(j) then (
    let tmp = t.(i) in
    t.(i) <- t.(j);
    t.(j) <- tmp
  )

let tri_3 (t:int array) : unit =
  ranger t 0 1;
  ranger t 0 2;
  ranger t 1 2

let tri_4 (t:int array) : unit =
  ranger t 0 1;
  ranger t 2 3;
  ranger t 0 2;
  ranger t 1 3;
  ranger t 1 2

let remplir_tableau (t:int array) (v,w,x,y,z) =
  t.(0) <- v; t.(1) <- w; t.(2) <- x; t.(3) <- y; t.(4) <- z

let tri_5 (t:int array) : unit =
  (* construction du diagramme *)
  ranger t 0 1; ranger t 2 3;
  let a, b, c, d = if t.(1) > t.(3)
    then t.(1), t.(3), t.(2), t.(0)
    else t.(3), t.(1), t.(0), t.(2)
  and e = t.(4) in

  (* insertion de e dans la liste, puis de d *)
  if e > b then
    if e > a then (* ordre c,b,a,e *)
      if d > b then
        remplir_tableau t (c,b,d,a,e)
      else
        if d > c then
          remplir_tableau t (c,d,b,a,e)
        else
          remplir_tableau t (d,c,b,a,e)
    else (* ordre c,b,e,a *)
      if d > b then
        if d > e then
          remplir_tableau t (c,b,e,d,a)
        else
          remplir_tableau t (c,b,d,e,a)
      else
        if d > c then
          remplir_tableau t (c,d,b,e,a)
        else
          remplir_tableau t (d,c,b,e,a)
  else
    if e > c then (* ordre c,e,b,a *)
      if d > e then
        if d > b then
          remplir_tableau t (c,e,b,d,a)
        else
          remplir_tableau t (c,e,d,b,a)
      else
        if d > c then
          remplir_tableau t (c,d,e,b,a)
        else
          remplir_tableau t (d,c,e,b,a)
    else (* ordre e,c,b,a *)
      if d > c then
        if d > b then
          remplir_tableau t (e,c,b,d,a)
        else
          remplir_tableau t (e,c,d,b,a)
      else
        if d > e then
          remplir_tableau t (e,d,c,b,a)
        else
          remplir_tableau t (d,e,c,b,a)
