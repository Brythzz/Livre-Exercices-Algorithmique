let partage (a:int array array) : int array =
  let n = Array.length a in
  let maison = Array.make n 0 in
  let vu = Array.make n false in

  for i=0 to n-1 do
    if not vu.(i) then ( (* nouvelle composante connexe *)
      maison.(i) <- 1;
      vu.(i) <- true;
      for j=i+1 to n-1 do (* ajout des personnes en conflit *)
        if a.(i).(j) = 1 then
          maison.(j) <- -1
      done;

      (* recherche d'un personne non vue dans une maison *)
      for j=i+1 to n-1 do
        if maison.(j) <> 0 && not vu.(j) then (
          (* on vérifie que j n'est pas en conflit *)
          for k=i+1 to n-1 do
            if k <> j && maison.(k) = maison.(j) && a.(k).(j) = 1 then
              failwith "partage impossible"
          done;

          (* on met les ennemis de j dans l'autre maison *)
          for k=i+1 to n-1 do
            if k <> j && a.(k).(j) = 1 then
              maison.(k) <- -maison.(j)
          done;

          vu.(j) <- true;
        )
      done
    )
  done;
  maison
