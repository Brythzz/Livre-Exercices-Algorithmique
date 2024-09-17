type arbre = Noeud of arbre * arbre | Feuille of char

let arbre1 =
Noeud(
  Noeud(
    Noeud(Feuille 'i', Feuille 'a'),
    Noeud(
      Noeud(Feuille 'f', Feuille 'l'),
      Feuille 'b'
    )
  ),
  Noeud(
    Feuille 'e',
    Noeud(Feuille 'c', Feuille 'd')
  )
)

let decode (message:string) (a:arbre) : string =
  let n = String.length message in
  (* le message contient au plus n caractères *)
  let buf = Buffer.create n in

  let rec parcours i noeud =
    match noeud with
    | Feuille x -> Buffer.add_char buf x;
      if i < n then parcours i a else Buffer.contents buf
    | Noeud (fg, fd) -> if message.[i] = '0' then
      parcours (i+1) fg else parcours (i+1) fd
  in parcours 0 a

let list_to_string (l:string list) : string =
  let n = List.length l in
  let buf = Buffer.create n in
  List.iter (Buffer.add_string buf) l;
  Buffer.contents buf

let encode (message:string) (a:arbre) : string =
  let h = Hashtbl.create 8 in
  let rec remplir a acc = match a with
    | Feuille x -> let code = list_to_string (List.rev acc) in
        Hashtbl.add h x code
    | Noeud (fg, fd) -> remplir fg ("0"::acc); remplir fd ("1"::acc)
  in remplir a [];

  let n = String.length message in
  let rec construire i acc =
    if i = n then list_to_string (List.rev acc)
    else let code = Hashtbl.find h message.[i] in
      construire (i+1) (code::acc)
  in construire 0 []

let string_to_int (message:string) : int =
  let k = ref (Int.shift_left 1 31) in  (* 2^31 *)
  let n= String.length message in

  let s = ref 0 in
  for i=0 to n-1 do
    if message.[i] = '1' then s := !s + !k;
    k := !k /2
  done; !s
let compact (message:string) : int list =
  let n = String.length message in

  let rec aux i acc =
    let len = if i=n/32 then n mod 32 else 32 in
    let s = String.sub message (32*i) len in
    let num = string_to_int s in
    if i=n/32 then num::acc else aux (i+1) (num::acc)
  in List.rev (aux 0 [])
