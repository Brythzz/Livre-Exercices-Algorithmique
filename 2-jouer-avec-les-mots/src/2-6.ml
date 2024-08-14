let longueur (a:string) (b:string) : int =
  let n = String.length a in
  let m = String.length b in
  let long = Array.make_matrix (n+1) (m+1) 0 in

  for i=1 to n do
    for j=1 to m do
      if a.[i-1] = b.[j-1] then
        long.(i).(j) <- long.(i-1).(j-1) + 1
      else
        long.(i).(j) <- max long.(i).(j-1) long.(i-1).(j)
    done
  done;
  long.(n).(m)

let plsc_chemin (a:string) (b:string) : string array array =
  let n = String.length a in
  let m = String.length b in
  let long = Array.make_matrix (n+1) (m+1) 0 in
  let chemin = Array.make_matrix (n+1) (m+1) "" in

  for i=1 to n do
    for j=1 to m do
      if a.[i-1] = b.[j-1] then (
        long.(i).(j) <- long.(i-1).(j-1) + 1;
        chemin.(i).(j) <- " ↖ "
      )
      else if long.(i-1).(j) >= long.(i).(j-1) then (
        long.(i).(j) <- long.(i-1).(j);
        chemin.(i).(j) <- " ↑ "
      )
      else (
        long.(i).(j) <- long.(i).(j-1);
        chemin.(i).(j) <- " ← "
      )
    done
  done;
  chemin

let plsc (a:string) (b:string) : unit =
  let n = String.length a in
  let m = String.length b in
  let chemin = plsc_chemin a b in

  let rec aux i j =
    if i=0 || j=0 then ()

    else if chemin.(i).(j) = " ↖ " then (
      aux (i-1) (j-1);
      print_char a.[i-1]
    )
    else if chemin.(i).(j) = " ↑ " then
      aux (i-1) j
    else
      aux i (j-1)
  in aux n m;
  print_newline ()
