let preferee (i:int) (g:int array array) : int =
  let n = Array.length g in
  let ordre = ref (n+1) and p = ref (-1) in
  for j=0 to n-1 do
    if g.(i).(j) < !ordre then (
      ordre := g.(i).(j);
      p := j
    )
  done;
  !p

let mariages (f:int array array) (g:int array array) : int array =
  let n = Array.length f in
  let fiance = Array.make n (-1) in
  let fiancee = Array.make n (-1) in

  let fini = ref false in
  while not !fini do
    (* recherche d'un garçon non fiancé *)
    let garcon = ref 0 in
    while !garcon < n && fiancee.(!garcon) <> (-1) do
      incr garcon
    done;
    if !garcon = n then fini := true else (
      let fille = preferee !garcon g in
      let rival = fiance.(fille) in

      if rival <> (-1) then (
        if f.(fille).(!garcon) < f.(fille).(rival) then (
          (* la fille préfère le garçon à son actuel fiancé *)
          (* on rompt les fiançailles et on en célèbre de nouvelles *)
          fiancee.(!garcon) <- fille; fiancee.(fille) <- !garcon;
          fiancee.(rival) <- (-1); g.(rival).(fille) <- n+1
          (* on enlève la fille des choix possibles du rival *)
        ) else (
          (* elle préfère son actuel fiancé : on l'enlève des *)
          (* choix possibles du garçon *)
          g.(!garcon).(fille) <- n+1
        )
      ) else (    (* il n'y a pas de rival *)
        fiancee.(!garcon) <- fille; fiance.(fille) <- !garcon
      )
    )
  done; (* à ce stade, tout le monde est fiancé,
           il ne reste plus qu'à célébrer les noces ! *)
  fiance
