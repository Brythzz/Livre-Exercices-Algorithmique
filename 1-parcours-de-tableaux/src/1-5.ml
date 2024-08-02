let ranger (t:int array) (i:int) (j:int) : unit =
  if t.(i) > t.(j) then (
    let tmp = t.(i) in
    t.(i) <- t.(j);
    t.(j) <- tmp
  )

let tri_3 (t:int array) : unit =
  ranger t 1 2;
  ranger t 1 3;
  ranger t 2 3

let tri_4 (t:int array) : unit =
  ranger t 1 2;
  ranger t 3 4;
  ranger t 1 3;
  ranger t 2 4;
  ranger t 2 3

