let gen_mat (n:int) (m:int) : int array array =
  let a = Array.make_matrix n m 0 in
  let p = ref 1 and k = ref 0 in    (* p=2^k *)

  (* Traitement des i compris entre deux puissances de 2 *)
  for i=1 to n do
    for j=1 to m do    (* Cas où i < 2^k *)
      if i <> !p && j = i - !k then
        a.(i-1).(j-1) <- 1
    done;
    if i = !p then (    (* Cas où i = 2^k, on passe à i < 2^(k+1) *)
      p := !p * 2; k := !k + 1
    )
  done;

  (* Traitement des i = 2^k *)
  p := 1; k := !k - 1;
  for i=0 to !k do
    let d = ref 1 and c = ref 0 in    (* d = 2^c *)
    for j=1 to n do
      if j = !d then (
        d := !d * 2; c := !c + 1
      )
      else if j mod (2 * !p) >= !p then
        a.(!p-1).(j - !c - 1) <- 1
    done;
    p := !p * 2
  done;
  a

let print_matrix mat =
  let n = Array.length mat in
  let m = Array.length mat.(0) in

  for i=0 to n-1 do
    for j=0 to m-1 do
      Printf.printf "%d " mat.(i).(j);
    done;
    print_newline ()
  done

let encode (v:int array) (a:int array array) : int array =
  let n = Array.length a in
  let m = Array.length a.(0) in
  let c = Array.make n 0 in

  for i=0 to n-1 do
    for j=0 to m-1 do
      c.(i) <- (c.(i) + a.(i).(j) * v.(j)) mod 2
    done
  done;
  c

let cont_mat (n:int) (r:int) : int array array =
  let h = Array.make_matrix r n 0 in
  let p = ref 1 in

  for i=1 to r do
    for j=1 to n do
      if j mod (2 * !p) >= !p then
        h.(i-1).(j-1) <- 1
    done;
    p := !p * 2
  done;
  h

let syndrome (c':int array) (h:int array array) : int =
  let n = Array.length c' in
  let r = Array.length h in
  let s = Array.make r 0 in

  for i=0 to r-1 do
    for j=0 to n-1 do
      s.(i) <- (s.(i) + h.(i).(j) * c'.(j)) mod 2
    done
  done;

  let d = ref 0 in let p = ref 1 in (* Conversion *)
  for i=0 to r-1 do
    d := !d + s.(i) * !p;
    p := 2 * !p;
  done;
  !d-1

let decode (c':int array) (h:int array array) : int array =
  (* Correction d'erreur *)
  let d = syndrome c' h in
  if d <> (-1) then c'.(d) <- 1 - c'.(d);

  (* Décodage *)
  let n = Array.length c' in
  let m = n - Array.length h in
  let message = Array.make m 0 in
  let p = ref 1 and r = ref 0 in
  for i=1 to n do
    if i <> !p then
      message.(i - !r - 1) <- c'.(i-1)
    else
      ( p := !p * 2; r := !r + 1 )
  done;
  message

(* Tests Hamming(7,4) *)
let _ =
  let n = 7 in (* bits totaux *)
  let m = 4 in (* bits de message *)
  let r = n-m in (* bits de parité *)

  let message = [|1;0;1;1|] in
  let a = gen_mat n m in
  let c = encode message a in

  (* erreur de transmission *)
  c.(0) <- 1 - c.(0);

  let h = cont_mat n r in
  let dec = decode c h in
  dec
