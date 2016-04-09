open Batteries

type comp =
  | Re of Re.re
  | Free
  | Empty
type pol = Pos | Neg
type patt = comp list * pol

let readdir root =
  Array.to_list @@ Sys.readdir root

let concat parent child =
  if parent = "." then child
  else Filename.concat parent child

let (@.) f g x = f (g x)

let good xs =
  not (List.fold_left (fun bad e -> bad || (e = Neg)) false xs)

(** [glob' patts path base] is used internally by [glob].
    [patts] is a compiled list of [patt]s as opposed to 
    It is used internally by [glob]. *)
let rec glob' (patts : patt list) path base =
  let is_dir = Sys.is_directory path in
  let applicable =
    if not is_dir then
      patts
      |> List.filter_map (function
        | [_], _ as p -> Some p
        | Free :: comps, pol -> Some (comps, pol)
        | _ -> None
      )
    else patts
  in
  let matching =
    applicable
    |> List.filter (function
      | Free :: _, _ -> true
      | Re re :: _, _ -> Re.execp re base
      | [Empty], _ | [], _ -> false
      | Empty :: _, _ -> failwith "Empty in middle of glob."
    )
  in
  if matching = [] then []
  else if not is_dir then
    if good @@ List.map snd matching then [path] else []
  else
    let self =
      let self_matches =
        matching
        |> List.filter_map (function
          | [_], pol -> Some pol
          | [_; Empty], pol -> Some pol
          | _ -> None
        )
      in
      good self_matches
    in
    if not self then []
    else
      let descending =
        matching
        |> List.filter_map (function
          | Re _ :: (_ :: _ as ps), pol -> Some [ps, pol]
          | Free :: rest as full, pol -> Some [rest, pol; full, pol]
          | _ -> None
        )
        |> List.flatten
      in
      let children =
        readdir path
        |> List.map (fun child ->
          glob' descending (concat path child) child
        )
        |> List.flatten
      in
      path :: children

(** [glob patts root] takes a list of glob patterns [patts] and tries to
    match them against files in [root] (or, if [root] is a file, [root]
    itself. *)
let glob patts root =
  let patts' =
    List.map (fun patt ->
      let pol, patt' =
        if patt.[0] = '!' then Neg, String.sub patt 1 (String.length patt - 1)
        else Pos, patt
      in
      let comps =
        String.nsplit patt' ~by:"/" 
        |> List.map (fun comp ->
          if comp = "**" then Free
          else if comp = "" then Empty
          else Re (Re.compile @@ Re_glob.glob ~anchored:true ~expand_braces:true comp)
        )
      in
      comps, pol
    ) patts
    |> List.map (fun ((comps, _) as patt) ->
      let n = List.length comps in
      List.iteri (fun i comp ->
        if comp = Empty && i < n - 1 then invalid_arg "Invalid pattern: empty component in middle."
        else ()
      ) comps ;
      patt
    )
  in
  if Sys.is_directory root then
    readdir root
    |> List.map (fun child ->
      glob' patts' (concat root child) child
    )
    |> List.flatten
  else glob' patts' "" root
