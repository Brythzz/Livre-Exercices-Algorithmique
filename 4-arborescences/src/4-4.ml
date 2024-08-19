type arbre =
  | Noeud of arbre * int * arbre
  | Feuille of char

let fusion (a1:arbre) (a2:arbre) (occ:int array) = match a1, a2 with
  | Noeud (_, p1, _), Noeud (_, p2, _) -> Noeud (a2, p1 + p2, a1)
  | Noeud (_, p1, _), Feuille c | Feuille c, Noeud (_, p1, _)
      -> let idx = int_of_char c - 97 in
      let p2 = occ.(idx) in Noeud (a1, p1 + p2, a2)
  | Feuille c1, Feuille c2 -> let i1 = int_of_char c1 - 97 in
    let i2 = int_of_char c2 - 97 in
    Noeud(a1, occ.(i1) + occ.(i2), a2)

let list_to_string (l:string list) : string =
  let n = List.length l in
  let buf = Buffer.create n in
  List.iter (Buffer.add_string buf) l;
  Buffer.contents buf

let stocker_codes (a:arbre) : (char, string) Hashtbl.t =
  let h = Hashtbl.create 8 in
  let rec remplir a acc = match a with
    | Feuille x -> let code = list_to_string (List.rev acc) in
        Hashtbl.add h x code
    | Noeud (fg, _, fd) -> remplir fg ("0"::acc); remplir fd ("1"::acc)
  in remplir a []; h
