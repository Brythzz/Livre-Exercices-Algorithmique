let est_ferme (poly:string) : bool =
  let n = String.length poly in
  let x = ref 0 and y = ref 0 in
  for i=0 to n-1 do
    match poly.[i] with
      | 'h' -> incr y
      | 'b' -> decr y
      | 'd' -> incr x
      | _ -> decr x
  done;
  (!x, !y) = (0,0)

let abscisses (poly:string) : (int * int) =
  let n = String.length poly in
  let x = ref 0  in
  let xmax = ref 0 and xmin = ref 0 in
  for i=0 to n-1 do
    match poly.[i] with
      | 'd' -> incr x; if !x > !xmax then xmax := !x
      | 'g' -> decr x; if !x < !xmin then xmin := !x
      | _ -> ()
  done;
  (!xmin, !xmax)

let ordonnees (poly:string) : (int * int) =
  let n = String.length poly in
  let y = ref 0  in
  let ymax = ref 0 and ymin = ref 0 in
  for i=0 to n-1 do
    match poly.[i] with
      | 'h' -> incr y; if !y > !ymax then ymax := !y
      | 'b' -> decr y; if !y < !ymin then ymin := !y
      | _ -> ()
  done;
  (!ymin, !ymax)

let dessin (poly:string) : int array array =
  let xmin, xmax = abscisses poly in
  let ymin, ymax = ordonnees poly in
  let n = ymax - ymin + 1 and m = xmax - xmin + 1 in
  let t = Array.make_matrix n m 0 in

  let l = String.length poly in
  let i = ref ymax and j = ref (-xmin) in
  t.(!i).(!j) <- 1;
  for k=0 to l-1 do
    (match poly.[k] with
      | 'd' -> incr j
      | 'g' -> decr j
      | 'h' -> decr i
      | _ -> incr i);
    t.(!i).(!j) <- 1
  done; t

let airesimple (poly:string) : int =
  let n = String.length poly in
  let aire = ref 0 in
  let x = ref 0 in
  for i=0 to n-1 do
    let dy = ref 0 in
    (match poly.[i] with
      | 'h' -> dy := 1
      | 'b' -> dy := -1
      | 'd' -> incr x
      | _ -> decr x);
    aire := !aire + !x * !dy
  done;
  abs !aire
