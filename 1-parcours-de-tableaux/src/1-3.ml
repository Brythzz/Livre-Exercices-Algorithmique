let recherche (t:int array) (p:int) (s:int) : unit =
  let n = Array.length t in
  for k=0 to n-p do
    let somme = ref 0 in
    for i=k to p+k do
      somme := !somme + t.(i)
    done;
    if !somme >= s then
      (print_int k; print_newline ())
  done

let recherche (t:int array) (p:int) (s:int) : unit =
  let n = Array.length t in
  let somme = ref 0 in
  for i=0 to p do
    somme := !somme + t.(i)
  done;
  if !somme >= s then print_endline "0";

  for k=1 to n-1-p do
    somme := !somme - t.(k-1) + t.(k+p);
    if !somme >= s then
      (print_int k; print_newline ())
  done

let somme (t:int array) (k:int) (p:int) =
  let somme = ref 0 in
  for i=k to p+k do
    somme := !somme + t.(i)
  done;
  !somme

(* Renvoie l'indice k à partir duquel on a la propriété *)
let dichotomie (t: int array) (p:int) (s:int) : int =
  let n = Array.length t in
  let min = ref 0 and max = ref (n-p-1) in
  while !min <= !max do
    let m = (!min + !max) / 2 in
    if somme t m p >= s
      then max := m - 1
      else min := m + 1
  done;
  !max + 1

let triplets (n:int) =
  for i=1 to n-1 do
    for j=1 to i-1 do
      let i' = float_of_int i in
      let sup = int_of_float (1.5 *. i') in
      for k=i+1 to sup do
        if i*i + j*j = k*k then
          Printf.printf "%d %d %d\n" i j k
      done
    done
  done

let triplets (n:int) =
  for i=1 to n-1 do
    for j=1 to i-1 do
      let r = float_of_int (i*i + j*j) in
      let k = Float.round (sqrt r) in
      if k*.k = r then
        Printf.printf "%d %d %f\n" i j k
    done
  done
