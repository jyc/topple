open Printf
open Topple

let graph_file = "_topple"
let status_file = ".topple-status"
let version_file = ".topple-version"

let verbose = ref false

let red s =
  sprintf "\x1b[31m%s\x1b[0m" s

let green s =
  sprintf "\x1b[32m%s\x1b[0m" s

let magenta s =
  sprintf "\x1b[35m%s\x1b[0m" s

let logv s =
  if !verbose then
    print_endline (magenta s)

let err_no_version vfile path =
  fprintf stderr "\x1b[31m\
                  Error: %s is empty, but %s is listed in %s. \
                  Run `topup '%s'` to create a version file or remove it.\
                  \x1b[0m\n"
    vfile path graph_file path;
  exit 1

let err_no_graph () =
  fprintf stderr "\x1b[31m\
                  Error: No %s file found in the current directory.\
                  \x1b[0m\n"
    graph_file ;
  exit 1

let err_unrecognized_direct path =
  fprintf stderr "\x1b[31m\
                  Error: %s is specified as a direct argument, but it either \
                  wasn't found in the \ graph file or its version file doesn't \
                  exist.\
                  \x1b[0m\n"
    path ;
  exit 1

let in_dir path thunk =
  let cwd = Unix.getcwd () in
  Unix.chdir path ;
  let x = thunk () in
  Unix.chdir cwd ;
  x

let rand_version () =
  let rand () =
    let n = Random.int 62 in
    if n < 26 then Char.code 'a' + n
    else if n < 52 then Char.code 'A' + n - 26
    else Char.code '0' + n - 52
  in
  let s = Bytes.make 32 ' ' in
  for i = 0 to 31 do
    s.[i] <- Char.chr (rand ())
  done ;
  Bytes.to_string s

let topup path =
  let vfile = Filename.concat path version_file in
  if not (Sys.file_exists vfile) then
    err_unrecognized_direct path
  else begin
    let ouch = open_out vfile in
    output_string ouch (rand_version ()) ;
    output_char ouch '\n' ;
    close_out ouch
  end

let run ~force ~direct =
  if not (Sys.file_exists graph_file) then err_no_graph () ;

  let graph = Graph.load graph_file in

  List.iter (fun path ->
    if not (Graph.mem graph path) then err_unrecognized_direct path
    else topup path
  ) direct ;

  let status = hashtbl_of_alist @@ load_status status_file in
  let update =
    Graph.paths graph
    |> BatList.filter_map (fun path ->
      let vfile = Filename.concat path version_file in
      if not (Sys.file_exists vfile) then err_no_version vfile path
      else
        begin match load_version vfile with
        | None -> err_no_version vfile path
        | Some version ->
          match Hashtbl.find status path with
          | version' ->
            if force || version' <> version then Some (path, version)
            else None
          | exception Not_found -> Some (path, version)
        end
    )
  in
  let dirty = List.map (fun (path, _) -> path) update in

  logv @@ sprintf "Dirty: %s" (String.concat ", " dirty) ;

  let affected = Graph.affected graph dirty in

  logv @@ sprintf "Running: %s" (String.concat " → " affected) ;

  let ran = Hashtbl.create 0 in
  let rec loop = function
    | [] -> ()
    | path :: paths ->
      let commands = Graph.commands graph path in
      let rec run_commands = function
        | [] -> true
        | cmd :: cmds ->
          printf "[%s] %s\n%!" (green path) cmd ;
          match Unix.system cmd with
          | Unix.WEXITED 0 -> run_commands cmds
          | Unix.WEXITED code ->
            fprintf stderr "[%s] '%s' returned with exit code %d.\n" (red path) cmd code ;
            false
          | _ ->
            fprintf stderr "[%s] '%s' exited unusually.\n" (red path) cmd ;
            false
      in
      let ok =
        in_dir path (fun () ->
          run_commands commands
        )
      in

      if ok then begin
        Hashtbl.replace ran path () ;
        loop paths
      end else ()
  in

  loop affected ;

  let retry =
    affected
    |> List.filter (fun path -> not (Hashtbl.mem ran path))
  in
  let successful =
    Hashtbl.fold (fun k () acc -> k :: acc) ran []
  in
  let updated =
    update
    |> List.filter (fun (path, _) -> Hashtbl.mem ran path)
  in

  logv @@ sprintf "Successful: %s" (String.concat ", " successful) ;
  logv @@ sprintf "To retry: %s" (String.concat ", " retry) ;

  update_status status_file updated retry

let () =
  Random.self_init () ;

  let force = ref false in
  let specs = Arg.align [
    "-v", Arg.Set verbose, " Output more.";
    "-f", Arg.Set force, " Run everything."
  ]
  in
  let usage_msg =
    sprintf "Usage: %s [direct] [options]\n\
             \  Specifying a path as a direct argument increases that path's .topple-version\n\
             \  file before processing.\n\
             Options:"
      Sys.argv.(0)
  in
  let direct = ref [] in
  Arg.parse specs (fun s -> direct := s :: !direct) usage_msg ;
  direct := List.rev !direct ;
  run ~force:!force ~direct:!direct
