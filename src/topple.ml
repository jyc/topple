open Printf

let with_input inp thunk =
  try
    let x = thunk () in
    close_in inp ;
    x
  with e ->
    close_in inp ;
    raise e

let with_output out thunk =
  try
    let x = thunk () in
    close_out out ;
    x
  with e ->
    close_out out ;
    raise e

module Graph = struct

  (* deps, commands *)
  type t = (string, string list * string list) Hashtbl.t

  let add ~commands ~deps g path =
    Hashtbl.replace g path (commands, deps)

  let deps g path =
    try fst @@ Hashtbl.find g path
    with Not_found -> []

  let commands g path =
    try snd @@ Hashtbl.find g path
    with Not_found -> []

  let mem g path =
    Hashtbl.mem g path

  let paths g =
    let marks : (string, unit) Hashtbl.t = Hashtbl.create 0 in
    let marked = Hashtbl.mem marks in
    let mark n = Hashtbl.replace marks n () in
    Hashtbl.fold (fun n (ms, _) paths ->
      if marked n then paths
      else begin
        mark n ;
        List.fold_left (fun paths m ->
          if marked m then paths
          else begin
            mark m ;
            m :: paths
          end
        ) (n :: paths) ms
      end
    ) g []

  let colon_re = Pcre.regexp ":"
  let ws_re = Pcre.regexp {|\s+|}
  let comment_re = Pcre.regexp {|^((?:[^\\#]|\\#|\\\\)*)(?:#.*)?$|}
  let comment_subst = Pcre.subst "$1"

  let load path =
    let inp = open_in path in
    let rec get_line () =
      try
        input_line inp 
        |> Pcre.replace ~rex:comment_re ~itempl:comment_subst
      with End_of_file ->
        raise BatEnum.No_more_elements
    in
    (* Wrap the input stream so we can peek and delete comments. *)
    let ins = BatEnum.from get_line in

    let rec read_path_deps path deps =
      let handle path deps_string =
        let deps_string = String.trim deps_string in
        let n = String.length deps_string in
        if n = 0 then (path, deps)
        else if deps_string.[n - 1] = '\\' then
          let deps' = String.sub deps_string 0 (n - 1) in
          read_path_deps (Some path) (Pcre.split ~rex:ws_re deps' @ deps)
        else (path, Pcre.split ~rex:ws_re deps_string @ deps)
      in
      match path with
      | None ->
        let s = BatEnum.get_exn ins in
        begin match Pcre.split ~rex:colon_re ~max:2 s with
        | [path; deps_string] -> handle path deps_string
        | _ -> failwith @@ sprintf "Expected '<path>: <dep> ...', got '%s'." s 
        end
      | Some path ->
        let s = BatEnum.get_exn ins in
        handle path s
    in

    let rec read_commands commands =
      let finish () =
        List.rev commands
      in
      match BatEnum.peek ins with
      | None | Some "" -> finish ()
      | Some s ->
        begin match s.[0] with
        | ' ' | '\t' ->
          BatEnum.junk ins ;
          read_commands (String.trim s :: commands)
        | _ ->
          finish ()
        end
    in

    let ht = Hashtbl.create 0 in
    let rec loop () =
      match BatEnum.peek ins with
      | None -> ()
      | Some "" ->
        BatEnum.junk ins ;
        loop ()
      | Some _ ->
        let path, deps = read_path_deps None [] in
        let commands = read_commands [] in
        Hashtbl.replace ht path (deps, commands) ;
        loop ()
    in

    with_input inp (fun () ->
      loop () ;
      ht
    )

  (* Oof... *)
  exception Chosen of string
  exception Cyclical of string

  let sort g =
    let unvisited : (string, unit) Hashtbl.t = Hashtbl.create 0 in
    let marks : (string, [`Unmarked | `Temporary | `Permanent]) Hashtbl.t = Hashtbl.create 0 in
    let sorted : string list ref = ref [] in
    let choose_unvisited () =
      try 
        Hashtbl.fold (fun k () () -> raise (Chosen k)) unvisited () ;
        assert false
      with Chosen x -> x
    in
    let mark = Hashtbl.replace marks in
    let rec visit n =
      match Hashtbl.find marks n with
      | `Permanent -> ()
      | `Temporary -> raise (Cyclical n)
      | `Unmarked ->
        mark n `Temporary ;
        List.iter (fun m ->
          visit m
        ) (deps g n) ;
        mark n `Permanent ;
        Hashtbl.remove unvisited n ;
        sorted := n :: !sorted
    in
    let rec loop () =
      if Hashtbl.length unvisited = 0 then ()
      else begin
        visit (choose_unvisited ()) ;
        loop ()
      end
    in
    Hashtbl.iter (fun n (deps, _) -> 
      Hashtbl.replace unvisited n () ;
      Hashtbl.replace marks n `Unmarked ;
      List.iter (fun m ->
        Hashtbl.replace unvisited m () ;
        Hashtbl.replace marks m `Unmarked ;
      ) deps
    ) g ;
    loop () ;
    List.rev !sorted

  let affected g ns =
    let g' : (string, string list) Hashtbl.t = Hashtbl.create 0 in
    let marks : (string, [`Unmarked | `Temporary | `Permanent]) Hashtbl.t = Hashtbl.create 0 in
    let sorted : string list ref = ref [] in
    let mark = Hashtbl.replace marks in

    let rec visit n =
      match Hashtbl.find marks n with
      | `Permanent -> ()
      | `Temporary -> raise (Cyclical n)
      | `Unmarked ->
        mark n `Temporary ;
        let ms =
          try Hashtbl.find g' n
          with Not_found -> []
        in
        List.iter (fun m ->
          visit m
        ) ms ;
        mark n `Permanent ;
        sorted := n :: !sorted
    in

    (* Create g' (g with directions of edges reversed. *)
    Hashtbl.iter (fun n (ms, _) ->
      List.iter (fun m ->
        let old =
          try Hashtbl.find g' m
          with Not_found -> []
        in
        Hashtbl.replace g' m (n :: old)
      ) ms
    ) g ;

    (* Initialize [marks]. *)
    Hashtbl.iter (fun n (ms, _) -> 
      Hashtbl.replace marks n `Unmarked ;
      List.iter (fun m ->
        Hashtbl.replace marks m `Unmarked ;
      ) ms
    ) g ;

    List.iter visit ns ;
    !sorted

end

let hashtbl_of_alist ls =
  let ht = Hashtbl.create 0 in
  List.iter (fun (k, v) ->
    Hashtbl.replace ht k v
  ) ls ;
  ht

let hashset_of_list ls =
  let ht = Hashtbl.create 0 in
  List.iter (fun x ->
    Hashtbl.replace ht x ()
  ) ls ;
  ht

let load_version path =
  let inp = open_in path in
  try with_input inp (fun () ->
    Some (input_line inp)
  ) with End_of_file -> None

let split_status_line s =
  let n = String.length s in
  match String.rindex s '\t' with
  | i_t when i_t > 0 && i_t < n - 1 ->
    let path = String.sub s 0 i_t in
    let version = String.sub s (i_t + 1) (n - i_t - 1) in
    Some (path, version)
  | _ ->
    None

let load_status path : (string * string) list =
  match open_in path with
  | exception Sys_error _ ->
    []
  | inp ->
    let rec loop status =
      match input_line inp with
      | s ->
        begin match split_status_line s with
        | Some x -> loop (x :: status)
        | None -> loop status
        end
      | exception End_of_file -> status
    in
    with_input inp (fun () ->
      loop []
    )

let update_status path update retry =
  let update' = hashtbl_of_alist update in
  let retry' = hashset_of_list retry in

  let lines = 
    try BatList.of_enum @@ BatFile.lines_of path
    with Sys_error _ -> []
  in
  let out = open_out path in

  let write path version =
    output_string out @@ sprintf "%s\t%s\n" path version
  in

  let passthru s = 
    output_string out s ;
    output_string out "\n"
  in

  let rec loop = function
    | s :: ss ->
      begin match split_status_line s with
      | Some (path, version) ->
        if Hashtbl.mem update' path then begin
          write path (Hashtbl.find update' path) ;
          Hashtbl.remove update' path
        end else if Hashtbl.mem retry' path then ()
        else passthru s
      | _ ->
        passthru s
      end ;
      loop ss
    | [] ->
      Hashtbl.iter (fun path version ->
        write path version
      ) update'
  in
  with_output out (fun () ->
    loop lines
  )

