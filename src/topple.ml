open Batteries
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
  type node = {
    path : string;
    deps : string list;
    commands : string list;
    (* If [globs] is the empty list, then we should look for a .topple-status
       file instead of using [hash_path].. *)
    globs : string list;
  }
  type t = (string, node) Hashtbl.t

  let create () =
    Hashtbl.create 17

  let add ~path ~deps ~commands ~globs g =
    Hashtbl.replace g path { path; deps; commands; globs }

  let path g path =
    (Hashtbl.find g path).path

  let deps g path =
    (Hashtbl.find g path).deps

  let commands g path =
    (Hashtbl.find g path).commands

  let globs g path =
    (Hashtbl.find g path).globs

  let mem g path =
    Hashtbl.mem g path

  let paths g =
    let marks : (string, unit) Hashtbl.t = Hashtbl.create 0 in
    let marked = Hashtbl.mem marks in
    let mark n = Hashtbl.replace marks n () in
    Hashtbl.fold (fun n { deps } paths ->
      if marked n then paths
      else begin
        mark n ;
        List.fold_left (fun paths m ->
          if marked m then paths
          else begin
            mark m ;
            m :: paths
          end
        ) (n :: paths) deps
      end
    ) g []

  let ws_re = Re.(compile (rep1 space))
  let comment_re = Re.(compile (seq [char '#'; rep any; eol]))

  let load path =
    let inp = open_in path in
    let rec get_line () =
      try
        input_line inp 
        |> Re.replace_string comment_re ~by:""
      with End_of_file ->
        raise Enum.No_more_elements
    in
    (* Wrap the input stream so we can peek and delete comments. *)
    let ins = Enum.from get_line in

    let rec read_path_deps path deps =
      let handle path deps_string =
        let deps_string = String.trim deps_string in
        let n = String.length deps_string in
        if n = 0 then (path, deps)
        else if deps_string.[n - 1] = '\\' then
          let deps' = String.sub deps_string 0 (n - 1) in
          read_path_deps (Some path) (Re.split ws_re deps' @ deps)
        else (path, Re.split ws_re deps_string @ deps)
      in
      match path with
      | None ->
        let s = Enum.get_exn ins in
        begin match String.split s ~by:":" with
        | path, deps_string -> handle path deps_string
        | exception Not_found -> failwith @@ sprintf "Expected '<path>: <dep> ...', got '%s'." s 
        end
      | Some path ->
        let s = Enum.get_exn ins in
        handle path s
    in

    let rec read_commands commands =
      let finish () =
        List.rev commands
      in
      match Enum.peek ins with
      | None | Some "" -> finish ()
      | Some s ->
        begin match s.[0] with
        | ' ' | '\t' ->
          Enum.junk ins ;
          read_commands (String.trim s :: commands)
        | _ ->
          finish ()
        end
    in

    let g = create () in
    let rec loop () =
      match Enum.peek ins with
      | None -> ()
      | Some "" ->
        Enum.junk ins ;
        loop ()
      | Some _ ->
        let globs =
          let next = Option.get @@ Enum.peek ins in
          let has_globs =
            (* If there's a colon in the line and it comes before the first
               space or the end of the line (ci < si), then this looks like:
                 roboerror:
               Otherwise it's a glob line (command lines come after). *)
            let ci = try String.index next ':' with Not_found -> max_int in
            let si = try String.index next ' ' with Not_found -> max_int in
            ci >= si
          in
          if has_globs then begin
            Enum.junk ins ;
            String.nsplit next ~by:" "
          end else []
        in
        let path, deps = read_path_deps None [] in
        let commands = read_commands [] in
        add ~path ~deps ~commands ~globs g ;
        loop ()
    in

    with_input inp (fun () ->
      loop () ;
      g
    )

  (* Oof... *)
  exception Chosen of string
  exception Cyclic of string

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
      | `Temporary -> raise (Cyclic n)
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
    Hashtbl.iter (fun n { deps } -> 
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
      | `Temporary -> raise (Cyclic n)
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
    Hashtbl.iter (fun n { deps } ->
      List.iter (fun m ->
        let old =
          try Hashtbl.find g' m
          with Not_found -> []
        in
        Hashtbl.replace g' m (n :: old)
      ) deps
    ) g ;

    (* Initialize [marks]. *)
    Hashtbl.iter (fun n { deps } -> 
      Hashtbl.replace marks n `Unmarked ;
      List.iter (fun m ->
        Hashtbl.replace marks m `Unmarked ;
      ) deps
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
    try List.of_enum @@ File.lines_of path
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

let hash_path globs path =
  let files =
    Glob.glob globs path
    |> List.sort String.compare
  in
  let t =
    List.fold_left (fun t path ->
      (* Exclude directories, because their mtimes can change when a file we've
         ignored is modified/created. *)
      if Sys.is_directory path then t
      else max t (Unix.stat path).Unix.st_mtime
    ) 0. files
  in
  Hashtbl.hash (t, files)
