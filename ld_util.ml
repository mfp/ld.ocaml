open Printf 

(************************** copied from netdynlink.ml ************************)

type dynunit = {
  name: string;
  crc: Digest.t;
  imports_cmi: (string * Digest.t) list;
  imports_cmx: (string * Digest.t) list;
  defines: string list;
}

type dynheader = {
  magic: string;
  units: dynunit list;
}

let dyn_magic_number = "Caml2007D001"

let dll_filename fname =
  if Filename.is_implicit fname then Filename.concat (Sys.getcwd ()) fname
  else fname

(************************** /copied from netdynlink.ml ***********************)

let debug = ref true

external ld_extract_headers : string -> string = "ld_extract_headers"

let extract_units filename =
  let dll = dll_filename filename in
    (* FIXME: wrap exception *)
    try
      let data = ld_extract_headers dll in
      let header : dynheader = Marshal.from_string data 0 in
        if header.magic <> dyn_magic_number then
          failwith (filename ^ " is not an OCaml shared library.");
        header.units
    with Failure msg ->
      failwith (sprintf "Ld_util.extract_units (%S): %s" filename msg)

module DEP =
struct
  type t = string * Digest.t
  let compare = compare
end

module DS = Set.Make(DEP)
module DM = Map.Make(DEP)
module M = Map.Make(String)

type lib = {
  lib_filename : string;
  lib_units : dynunit list;
}

type catalog = {
  cat_intf_map : lib list DM.t;
}

type state = {
  st_libs : lib list;
  st_impls : Digest.t M.t;
  st_intfs : Digest.t M.t;
}

let empty_state = { st_libs = []; st_impls = M.empty; st_intfs = M.empty; }
let empty_catalog = { cat_intf_map = DM.empty }

let (|>) x f = f x

let add_dep state s d =
  (* FIXME: omit base modules and those in state *)
  (* FIXME: raise Not_found if dep not compatible with those in state *)
  DS.add d s

let check_conflicts deps tbl =
  List.iter
    (fun (name, digest) ->
       if M.mem name tbl && M.find name tbl <> digest then
         raise Not_found)
    deps

let add_lib lib st =
  {
    st_libs = st.st_libs @ [lib];
    st_impls =
      List.fold_left (fun m u -> M.add u.name u.crc m) st.st_impls lib.lib_units;
    st_intfs =
      List.fold_left
        (fun m u ->
           let digest = List.assoc u.name u.imports_cmi in
             M.add u.name digest m)
        st.st_intfs
        lib.lib_units;
  }

let check_lib_conflicts state lib =
  (* see if any of the units is already loaded *)
  List.iter
    (fun u -> if M.mem u.name state.st_intfs then raise Not_found) lib.lib_units

let map_concat f l = List.concat (List.map f l)

let update_deps all_cmis lib =
  (* remove cmis from list *)
  let libcmis =
    List.fold_left
      (fun s u -> DS.add (u.name, List.assoc u.name u.imports_cmi) s)
      DS.empty lib.lib_units in
  let cmis = List.filter (fun dep -> not (DS.mem dep libcmis)) all_cmis in
  let cmxs = map_concat (fun u -> u.imports_cmx) lib.lib_units in
    (cmis, cmxs)

let rec solve_dependencies cat state = function
      [], _ -> (* no cmi deps left *) state
    | ((cmi :: cmis) as all_cmis), all_cmxs ->
        check_conflicts all_cmis state.st_intfs;
        check_conflicts all_cmxs state.st_impls;
        match DM.find cmi cat.cat_intf_map with
            [] -> raise Not_found
          | l ->
              let rec loop = function
                  [] -> raise Not_found
                | lib :: libs ->
                    try
                      check_lib_conflicts state lib;
                      solve_dependencies cat
                        (add_lib lib state)
                        (update_deps all_cmis lib)
                    with Not_found -> loop libs
              in loop l

let run cat file =
  let units = extract_units file in
  let cmis = map_concat (fun u -> u.imports_cmi) units in
  let cmxs = map_concat (fun u -> u.imports_cmx) units in
  let solution = solve_dependencies cat empty_state (cmis, cmxs) in
    List.iter
      (fun lib ->
         try
           Dynlink.loadfile lib.lib_filename
         with Dynlink.Error e ->
           printf "Dynlink error when loading %s\n" lib.lib_filename;
           print_endline (Dynlink.error_message e);
           exit (-1))
      solution.st_libs

let is_cmxs s =
  let len = String.length s in
    len >= 5 && String.sub s (len - 5) 5 = ".cmxs"

let subdirs dir =
  try
    let fs = List.map (Filename.concat dir) (Array.to_list (Sys.readdir dir)) in
      List.filter Sys.is_directory fs
  with Sys_error _ -> []

let default_dirs =
  ["."; "/usr/lib/ocaml"; "/usr/local/lib/ocaml"] @
  subdirs "/usr/lib/ocaml" @ subdirs "/usr/local/lib/ocaml"

let build_catalog ?(dirs = default_dirs) () =
  let cmxs_files =
    map_concat
      (fun dir ->
         try
           let files = Array.to_list (Sys.readdir dir) in
             List.map (Filename.concat dir) (List.filter is_cmxs files)
         with Sys_error _ -> [])
      dirs
  in
    List.fold_left
      (fun cat file ->
         try
           if !debug then printf "Scanning %s\n" file;
           let units = extract_units file in
           let cmis =
             List.map (fun u -> (u.name, List.assoc u.name u.imports_cmi)) units in
           let lib = { lib_filename = file; lib_units = units; } in
             { cat_intf_map =
                 List.fold_left
                   (fun m intf ->
                      let prev = try DM.find intf m with Not_found -> [] in
                        DM.add intf (lib :: prev) m)
                   cat.cat_intf_map
                   cmis}
         with _ -> cat)
      empty_catalog
      cmxs_files
