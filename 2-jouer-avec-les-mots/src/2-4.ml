let est_facteur (u:string) (v:string) : bool =
  let n = String.length u in
  let k = String.length v in

  try
    for i=0 to n-k do
      let sub = String.sub u i k in
      if String.equal sub v then
        raise Exit
    done;
    false
  with Exit -> true

let equal (s1:string) (s2:string) : bool =
  let n = String.length s1 in
  let k = String.length s2 in
  if n <> k then false else
  try
    for i=0 to n-1 do
      if s1.[i] <> s2.[i]
        then raise Exit
    done;
    true
  with Exit -> false

let calcul (v:string) : int array =
  let k = String.length v in
  let b = Array.make k 0 in

  for h=1 to k-1 do
    let j = ref b.(h-1) in
    let fini = ref false in
    while not !fini do
      (* v_{j+1} = v_h *)
      if v.[!j] = v.[h] then (
        fini := true;
        b.(h) <- !j+1
      )
      else if !j = 0 then (
        fini := true;
        b.(h) <- 0
      )
      else j := b.(!j-1)
    done;
  done;
  b

let b (v:string) (n:int) : int =
  let b = calcul v in b.(n)

let est_facteur_bis (u:string) (v:string) : bool =
  let n = String.length u in
  let k = String.length v in

  let i = ref 0 and j = ref 0 in
  while !i < n && !j < k do
    if u.[!i] = v.[!j] then (
      incr i; incr j
    ) else (
      if !j = 0 then incr i
      else i := b v (!j-1)
    )
  done;
  !j = k
