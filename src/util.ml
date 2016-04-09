let partition f xs =
  let rec partition' f xs bs cs =
    match xs with
    | [] -> bs, cs
    | x :: xs ->
      begin match f x with
        | `Left b -> partition' f xs (b :: bs) cs
        | `Right c -> partition' f xs bs (c :: cs)
        | `Discard -> partition' f xs bs cs
      end
  in
  let (bs, cs) = partition' f xs [] [] in
  (List.rev bs, List.rev cs)

