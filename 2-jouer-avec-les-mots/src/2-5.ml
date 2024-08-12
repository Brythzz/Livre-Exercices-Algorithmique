let test (u:string) : bool =
  let n = String.length u in
  let cpt = ref 0 in
  try
    for i=0 to n-1 do
      if u.[i] = '(' then incr cpt
      else decr cpt;   (* u.[i] = ')' *)

      if !cpt < 0 then raise Exit
    done;
    !cpt = 0
  with Exit -> false

let decompose (u:string) : (string * string) =
  let n = String.length u in
  let cpt = ref 0 and idx = ref 0 in

  for i=0 to n-1 do
    if u.[i] = '(' then incr cpt
    else decr cpt;

    if !cpt = 0 && !idx = 0 then (
      idx := i
    )
  done;

  let v = String.sub u 1 (!idx-1) in
  let w = String.sub u (!idx+1) (n - !idx - 1) in
  (v, w)

let enumere_mots (l:int) =
  