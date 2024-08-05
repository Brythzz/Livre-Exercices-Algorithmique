let rec motdepliage (n:int) : (int list) =
  if n = 0 then [0] else
  let pred = motdepliage (n-1) in
  let pred' = List.map (fun e -> 1-e) (List.rev pred) in
  pred @ (0::pred')

let binaire (n:int) : (int list) =
  let rec aux n acc =
    if n = 0 then acc
    else aux (n / 2) ((n mod 2) :: acc)
  in
  if n = 0 then [0] else aux n []

let bit (nbin:int list) : int =
  let rev = List.rev nbin in
  let rec aux bin = match bin with
    | 1::1::_ -> 1
    | 1::0::_ | [1] -> 0
    | 0::q -> aux q
    | _ -> failwith "entrée invalide"
  in aux rev
