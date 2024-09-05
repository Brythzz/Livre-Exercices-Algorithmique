let cos = [|[|0.|]|] and d = [|[|0.|]|] and b = [|[|0.|]|]

let mat_dct (pix:int array array) : float array array =
  let dct = Array.make_matrix 8 8 0. in
  for i=0 to 7 do
    for j=0 to 7 do
      for x=0 to 7 do
        for y=0 to 7 do
          dct.(i).(j) <- dct.(i).(j) +. float_of_int pix.(x).(y)
            *. cos.(x).(i) *. cos.(y).(j);
        done
      done
    done
  done;
  dct

let mat_dct (pix:int array array) : float array array =
  let dct = Array.make_matrix 8 8 0. in
  let int = Array.make_matrix 8 8 0. in
  for i=0 to 7 do
    for j=0 to 7 do
      for k=0 to 7 do
        int.(i).(j) <- int.(i).(j) +. float_of_int pix.(i).(k)
          *. d.(j).(k)
      done
    done
  done;
  for i=0 to 7 do
    for j=0 to 7 do
      for k=0 to 7 do
        dct.(i).(j) <- dct.(i).(j) +. d.(i).(k) *. int.(k).(j)
      done
    done
  done;
  for i=0 to 7 do
    for j=0 to 7 do
      dct.(i).(j) <- dct.(i).(j) *. 0.25
    done
  done;
  dct

let mat_dct (pix:int array array) : float array array =
  let dct = Array.make_matrix 8 8 0. in
  let pax = Array.make_matrix 8 8 0 in
  (* multiplication par A *)
  for i=0 to 3 do
    for j=0 to 7 do
      pax.(i).(j) <- pix.(i).(j) - pix.(7-i).(j);
      pax.(7-i).(j) <- pix.(i).(j) + pix.(7-i).(j)
    done
  done;
  for i=0 to 7 do
    for j=0 to 3 do
      pax.(i).(j) <- pix.(i).(j) - pix.(i).(7-j);
      pax.(i).(7-j) <- pix.(i).(j) + pix.(i).(7-j)
    done
  done;
  (* multiplication par B *)
  let int = Array.make_matrix 8 8 0. in
  for i=0 to 7 do
    for j=0 to 7 do
      let t = if j mod 2 = 0 then 4 else 0 in
      for k=0 to 3 do
        int.(i).(j) <- int.(i).(j)
          +. float_of_int pax.(i).(k+t) *. b.(j).(k+t)
      done
    done
  done;
  for i=0 to 7 do
    for j=0 to 7 do
      let t = if j mod 2 = 0 then 4 else 0 in
      for k=0 to 3 do
        dct.(i).(j) <- dct.(i).(j) +. b.(i).(t+k) *. int.(k+t).(j)
      done
    done
  done;
  dct
