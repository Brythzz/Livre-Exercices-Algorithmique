let rom_to_dec (r:char array) : int array =
  let n = Array.length r in

  (* Création du tableau des valeurs *)
  let valeurs = Array.make n 0 in
  for i=0 to n-1 do
    valeurs.(i) <- match r.(i) with
      | 'I' -> 1
      | 'V' -> 5
      | 'X' -> 10
      | 'L' -> 50
      | 'C' -> 100
      | 'D' -> 500
      | _   -> 1000
  done;

  (* Conversion en entier *)
  let entier = ref valeurs.(n-1) in
  for i=0 to n-2 do
    if valeurs.(i) >= valeurs.(i+1)
      then entier := !entier + valeurs.(i)
    else entier := !entier - valeurs.(i)
  done;

  (* Conversion de l'entier en tableau de chiffres,
      poids faible en tête *)
  let d = Array.make 4 0 in
  let rec aux entier i =
    if entier <> 0 then (
      d.(i) <- entier mod 10;
      aux (entier/10) (i+1)
    )
  in aux !entier 0;

  (* Inversion du tableau pour mettre les poids forts en tête *)
  let tmp = d.(0) in
  d.(0) <- d.(3); d.(3) <- tmp;
  let tmp = d.(1) in
  d.(1) <- d.(2); d.(2) <- tmp;
  d

let dec_to_rom (d:int array) : char array =
  let r = Array.make 15 '0' in
  let j = ref 0 in

  let chiffre (x:char) (v:char) (i:char) (k:int) : unit =
    match k with
      | 1 -> r.(!j) <- i; j := !j+1
      | 2 -> r.(!j) <- i; r.(!j+1) <- i; j := !j+2
      | 3 -> r.(!j) <- i; r.(!j+1) <- i; r.(!j+2) <- i; j := !j+3
      | 4 -> r.(!j) <- i; r.(!j+1) <- v; j := !j+2
      | 5 -> r.(!j) <- v; j := !j+1
      | 6 -> r.(!j) <- v; r.(!j+1) <- i; j := !j+2
      | 7 -> r.(!j) <- v; r.(!j+1) <- i; r.(!j+2) <- i; j := !j+3
      | 8 -> r.(!j) <- v; r.(!j+1) <- i; r.(!j+2) <- i; r.(!j+3) <- i;
             j := !j+4
      | 9 -> r.(!j) <- i; r.(!j+1) <- x; j := !j+2
      | _ -> ()
  in
  (* On traduit les chiffres un à un *)
  chiffre 'M' 'M' 'M' d.(0);
  chiffre 'M' 'D' 'C' d.(1);
  chiffre 'C' 'L' 'X' d.(2);
  chiffre 'X' 'V' 'I' d.(3);
  Array.sub r 0 !j

let tab_to_int (d:int array) : int =
  let n = Array.length d in
  let entier = ref d.(0) in
  for i=1 to n-1 do
    entier := 10 * !entier + d.(i)
  done;
  !entier

let add (r1:char array) (r2:char array) =
  (* Conversion en tableaux de chiffres *)
  let d1 = rom_to_dec r1 in
  let d2 = rom_to_dec r2 in
  (* Conversion en entier *)
  let n1 = tab_to_int d1 in
  let n2 = tab_to_int d2 in
  let somme = n1 + n2 in
  (* Conversion de l'entier somme en tableau de chiffres,
    poids faible en tête *)
  let d = Array.make 4 0 in
  let rec aux entier i =
    if entier <> 0 then (
      d.(i) <- entier mod 10;
      aux (entier/10) (i+1)
    )
  in aux somme 0;

  (* Inversion du tableau pour mettre les poids forts en tête *)
  let tmp = d.(0) in
  d.(0) <- d.(3); d.(3) <- tmp;
  let tmp = d.(1) in
  d.(1) <- d.(2); d.(2) <- tmp;

  (* Conversion en caractères romains *)
  dec_to_rom d
