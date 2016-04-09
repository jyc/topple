(* Taken from Indoor_fs. *)

type tree = 
  | Directory of string * tree list
  | File of string

let fold f a = function
  | Directory (_, children) as dir ->
    List.fold_left f (f a dir) children
  | File _ as file ->
    f a file

let top = function
  | Directory (path, _) -> path
  | File path -> path

let pred_opt = function
  | None -> None
  | Some d -> Some (d - 1)

(* Taken from Indoor_util. *)

let trees ?depth ?(all_dirs=false) ~pred root =
  let rec read_dir' ?depth fs_root root =
    match depth with
    | Some d when d <= 0 -> Directory (root, [])
    | None | Some _ ->
      let entries =
        let files_arr = Sys.readdir fs_root in
        let () = Array.sort String.compare files_arr in
        Array.to_list files_arr
      in
      let depth' = pred_opt depth in
      let dirs, files =
        Util.partition
          (fun entry ->
             let fs_path = Filename.concat fs_root entry in
             let path = Filename.concat root entry in
             (* This is possible. Not sure why. *)
             if not (Sys.file_exists fs_path) then `Discard
             else if Sys.is_directory fs_path && (all_dirs || pred path) then `Left entry
             else if pred path then `Right entry
             else `Discard)
          entries
      in
      Directory (root,
                 List.map (fun s -> File (Filename.concat root s)) files @
                 List.map (fun d ->
                   read_dir' ?depth:depth' (Filename.concat fs_root d) (Filename.concat root d)
                 ) dirs)
  in
  match read_dir' ?depth root "" with
  | Directory (_, fs) -> fs
  | _ -> failwith "Invalid state."
