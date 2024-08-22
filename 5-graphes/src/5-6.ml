let pred_succ (c:bool array array) :
    (int array * int array * bool array array) =
  let n = Array.length c in
  let pred = Array.make_matrix n n false in
  let nb_pred = Array.make n 0 in
  let nb_succ = Array.make n 0 in

  for i=0 to n-1 do
    for j=0 to n-1 do
      if c.(i).(j) then (
        nb_succ.(i) <- nb_succ.(i) + 1;
        nb_pred.(j) <- nb_pred.(j) + 1;
        pred.(j).(i) <- true
      )
    done
  done;
  nb_pred, nb_succ, pred

let une_machine (c:bool array array) (dur:int array) : int array =
  let n = Array.length c in
  let deb = Array.make n (-1) in
  let nb_pred, _, _ = pred_succ c in

  let taches_faites = ref 0 and top = ref 0 in
  while !taches_faites < n do
    for i=0 to n-1 do
      (* exécution des tâches sans prédécesseurs *)
      if nb_pred.(i) = 0 && deb.(i) = (-1) then (
        (* exécution de t_i *)
        deb.(i) <- !top;
        top := !top + dur.(i);
        incr taches_faites;

        for j=0 to n-1 do    (* mise à jour du graphe *)
          if c.(i).(j) then
            nb_pred.(j) <- nb_pred.(j) - 1
        done
      )
      done
  done; deb
