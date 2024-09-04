let cos = [|[|0|]|]

let mat_dct (pix:int array array) =
  let dct = Array.make_matrix 8 8 0 in
  for i=0 to 7 do
    for j=0 to 7 do
      for x=0 to 7 do
        for y=0 to 7 do
          dct.(i).(j) <- dct.(i).(j)
            + pix.(x).(y) * cos.(x).(i) * cos.(y).(j);
        done
      done
    done
  done;
  dct
