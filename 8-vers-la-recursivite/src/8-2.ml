type matrix = float array array
let produit (a:matrix) (b:matrix) : matrix =
  let p = Array.length a in
  let q = Array.length b in
  let r = Array.length b.(0) in
  let c = Array.make_matrix p r 0. in
  for i=0 to p-1 do
    for j=0 to r-1 do
      for k=0 to q-1 do
        c.(i).(j) <- c.(i).(j) +. a.(i).(k) *. b.(k).(j)
      done
    done
  done;
  c

let nb_min_multiplications (r:int array) : int =
  let n = Array.length r - 1 in
  let m = Array.make_matrix n n 0 in

  for l = 2 to n do
    for i = 0 to n - l do
      let j = i + l - 1 in
      m.(i).(j) <- max_int;
      for k = i to j - 1 do
        let q = m.(i).(k) + m.(k+1).(j) + r.(i) * r.(k+1) * r.(j+1) in
        if q < m.(i).(j) then
          m.(i).(j) <- q
      done
    done
  done;
  m.(0).(n-1)
