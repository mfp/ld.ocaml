open Printf
open Ld_header

let debug = ref 0

external ld_extract_headers : string -> string = "ld_extract_headers"
external ld_plugin_header_offset : string -> int = "ld_get_caml_plugin_header_offset"

let matches re s =
  try
    ignore (Str.search_forward re s 0); true
  with Not_found -> false

let close_finally f ic =
  try let y = f ic in close_in ic; y with e -> close_in ic; raise e

let hex_to_i64 s = Int64.of_string ("0x" ^ s)
let (+!) = Int64.add
let (-!) = Int64.sub

(* when we cannot get the symbol with dlopen, because some symbol in the .cmxs
 * cannot be resolved (e.g. there's a reference to a primitive defined in
 * another library), so we locate the offset of caml_plugin_header in the file
 * manually using BFD *)
let manual_header_extraction filename : dynheader =
  match ld_plugin_header_offset filename with
      -1 -> failwith (sprintf "Could not find caml_plugin_header for %S." filename)
    | off ->
        close_finally
          (fun ic -> ignore (seek_in ic off); input_value ic)
          (open_in filename)

let extract_headers file : dynheader =
  try
    Marshal.from_string (ld_extract_headers file) 0
  with _ ->
    manual_header_extraction file

let extract_units filename =
  let dll = dll_filename filename in
    try
      let header = extract_headers dll in
        if header.magic <> dyn_magic_number then
          failwith (filename ^ " is not an OCaml shared library.");
        header.units
    with
      |  Failure msg ->
          failwith (sprintf "Ld_util.extract_units (%S): %s" filename msg)
      | e ->
          failwith (sprintf "Ld_util.extract_units (%S): %s" filename
                      (Printexc.to_string e))

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
  st_runtime_intfs : Digest.t M.t;
  st_runtime_impls : Digest.t M.t;
  st_impls : Digest.t M.t;
  st_intfs : Digest.t M.t;
  st_missing_intfs : DS.t;
}

let find_default d k m = try DM.find k m with Not_found -> d

let empty_catalog = { cat_intf_map = DM.empty }

let merge_catalogs l =
  let module S =
    Set.Make(struct
               type t = lib
               let compare l1 l2 = String.compare l1.lib_filename l2.lib_filename
             end) in
  let uniq l = S.elements (List.fold_left (fun s x -> S.add x s) S.empty l) in
  let intf_map =
    List.fold_left
      (fun m { cat_intf_map = m' } ->
         DM.fold (fun k l m -> DM.add k (uniq (l @ find_default [] k m)) m) m' m)
      DM.empty
      l
  in { cat_intf_map = intf_map }

let hex_to_digest s =
  let digest = String.create 16 in
    for i = 0 to 15 do
      Scanf.sscanf (String.sub s (2*i) 2) "%x" (fun n -> digest.[i] <- char_of_int n)
    done;
    digest

let state_of_known_modules ~known_interfaces ~known_implementations =
  let build_map l =
    List.fold_left
      (fun m (name, hex) -> M.add name (hex_to_digest hex) m)
      M.empty
      l
  in
    {
      st_libs = [];
      st_runtime_impls = build_map known_implementations;
      st_runtime_intfs = build_map known_interfaces;
      st_impls = M.empty;
      st_intfs = M.empty;
      st_missing_intfs = DS.empty;
    }

let empty_state st =
  { st with st_libs = []; st_impls = M.empty; st_intfs = M.empty }

let (|>) x f = f x

let check_conflicts ~msg deps tbl =
  List.iter
    (fun (name, digest) ->
       if M.mem name tbl && M.find name tbl <> digest then begin
         if !debug >= 2 then
           eprintf
             "%s conflict: %s %s vs. %s\n"
             msg name (Digest.to_hex (M.find name tbl)) (Digest.to_hex digest);
         raise Not_found
       end)
    deps

let add_lib lib st =
  if List.mem lib st.st_libs then st
  else
  {
    st with
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
    (fun map ->
       List.iter (fun u -> if M.mem u.name map then raise Not_found) lib.lib_units)
    [ state.st_intfs; state.st_runtime_intfs ]

let map_concat f l = List.concat (List.map f l)

let remove_satisfied_deps state cmis =
  let intfs = state.st_intfs in
  let runtime = state.st_runtime_intfs in
  List.filter
    (fun (name, digest) ->
       if not (M.mem name intfs || M.mem name runtime) then true
       else
         if M.mem name intfs && M.find name intfs = digest ||
            M.mem name runtime && M.find name runtime = digest then
           false
         else
           raise Not_found)
    cmis

let uniq_deps l = DS.elements (List.fold_left (fun s d -> DS.add d s) DS.empty l)

let lib_deps lib =
  let module S = Set.Make(String) in
  let libcmis = List.fold_left (fun s u -> S.add u.name s) S.empty lib.lib_units in
  (* must remove list of modules provided by the lib, libcmis, from its deps *)
  let remove_self_mods = List.filter (fun (name, _) -> not (S.mem name libcmis)) in
  let cmis = map_concat (fun u -> remove_self_mods u.imports_cmi) lib.lib_units  in
  let cmxs = map_concat (fun u -> u.imports_cmx) lib.lib_units in
    (uniq_deps cmis, cmxs)

let rec solve_dependencies ?parent cat state (cmis, cmxs) =
  let cmis = remove_satisfied_deps state cmis
  in match (cmis, cmxs) with
      [], _ -> (* no cmi deps left *) state
    | (((name, digest) as cmi) :: rest_cmis) as all_cmis, all_cmxs ->
        check_conflicts ~msg:"CMI" all_cmis state.st_intfs;
        check_conflicts ~msg:"CMX" all_cmxs state.st_impls;
        match find_default [] cmi cat.cat_intf_map with
            [] ->
                if !debug >= 1 then
                  eprintf "No implementation found for CMI %s (%s)%s\n"
                    name (Digest.to_hex digest)
                    (match parent with
                         None -> ""
                       | Some p ->sprintf "\n needed for %s" p);
                (* would be  raise Not_found  if all imported CMIs were
                 * guaranteed to be provided by some lib, but some aren't
                 * (e.g. Calendar_sig) since they only contain signatures and
                 * type defs *)
                (* so we just ignore the cmi and hope for the best --- we'll
                 * get a load-time error in the worst case *)
                solve_dependencies cat
                  ?parent
                  { state with st_missing_intfs = DS.add cmi state.st_missing_intfs }
                  (rest_cmis, all_cmxs)
          | (hd :: _) as l ->
              let rec loop = function
                  [] ->
                    if !debug >= 2 then
                      eprintf "No way to load %s(%s).\n"
                        (fst cmi) (Digest.to_hex (snd cmi));
                    raise Not_found
                | lib :: libs ->
                    let valid_lib =
                      try
                        if !debug >= 2 then
                          eprintf "Trying to satisfy %s(%s) with %s.\n"
                            name (Digest.to_hex digest) lib.lib_filename;
                        check_lib_conflicts state lib;
                        Some lib
                      with Not_found ->
                        if !debug >= 2 then
                          eprintf "Rejected %s for %s(%s).\n"
                            lib.lib_filename name (Digest.to_hex digest);
                        None
                    in match valid_lib with
                        None -> loop libs
                      | Some lib ->
                          let state =
                            let parent = lib.lib_filename in
                              add_lib lib (solve_dependencies ~parent cat state (lib_deps lib))
                          in solve_dependencies ?parent cat state (rest_cmis, all_cmxs)
              in loop l

let unresolved_modules st = DS.elements st.st_missing_intfs

let resolve cat state files =
  let t0 = Unix.gettimeofday () in
  let units = map_concat extract_units files in
  let exclude_self u = List.filter (fun (name, _) -> name <> u.name) in
  let cmis = map_concat (fun u -> exclude_self u u.imports_cmi) units in
  let cmxs = map_concat (fun u -> exclude_self u u.imports_cmx) units in
    if !debug >= 1 then
      eprintf "Needed %3.2fms to extract dependencies.\n"
        ((Unix.gettimeofday () -. t0) *. 1000.);
    let t0 = Unix.gettimeofday () in
    let sol = solve_dependencies ~parent:(String.concat ", " files)
                cat state (uniq_deps cmis, cmxs)
    in
      if !debug >= 1 then
        eprintf "Needed %3.2fms for symbol resolution.\n"
          ((Unix.gettimeofday () -. t0) *. 1000.);
      sol

let do_load solution file =
  try
    Dynlink.loadfile file
  with Dynlink.Error e ->
    printf "Dynlink error when loading %s\n" file;
    print_endline (Dynlink.error_message e);
    print_newline ();
    print_endline "The following symbols could not be resolved:";
    List.iter
      (fun (n, d) -> printf "%-30s %s\n" n (Digest.to_hex d))
      (unresolved_modules solution);
    exit (-1)

let load_deps solution =
  List.iter (fun lib -> do_load solution lib.lib_filename) solution.st_libs

let run cat state files =
  let solution = resolve cat state files in
    load_deps solution;
    List.iter (do_load state) files

let is_cmxs s =
  let len = String.length s in
    len >= 5 && String.sub s (len - 5) 5 = ".cmxs"

let subdirs dir =
  try
    let fs = List.map (Filename.concat dir) (Array.to_list (Sys.readdir dir)) in
      List.filter Sys.is_directory fs
  with Sys_error _ -> []

let default_dirs =
  ["/usr/lib/ocaml"; "/usr/local/lib/ocaml"] @
    subdirs "/usr/lib/ocaml" @ subdirs "/usr/local/lib/ocaml"

let catalog_magic_number = "ld.ocaml2009D001"
type saved_catalog = string * catalog

let save_catalog cat filename =
  let oc = open_out_bin filename in
    Marshal.to_channel oc (catalog_magic_number, cat) [];
    close_out oc

let load_catalog filename =
  let ic = open_in filename in
  let ((magic, cat) : saved_catalog) = Marshal.from_channel ic in
    close_in ic;
    if magic <> catalog_magic_number then
      failwith (sprintf
                  "Ld_util.load_catalog: %S is not a valid DLL catalog."
                  filename);
    cat

let build_catalog dirs =
  let module S = Set.Make(String) in
  let uniq l = S.elements (List.fold_left (fun s x -> S.add x s) S.empty l) in
  let cmxs_files =
    map_concat
      (fun dir ->
         try
           let files = Array.to_list (Sys.readdir dir) in
             List.map (Filename.concat dir) (List.filter is_cmxs files)
         with Sys_error _ -> [])
      (uniq dirs)
  in
    List.fold_left
      (fun cat file ->
         try
           if !debug >= 2 then eprintf "Scanning %s\n" file;
           let units = extract_units file in
           let cmis =
             List.map (fun u -> (u.name, List.assoc u.name u.imports_cmi)) units in
           let lib = { lib_filename = file; lib_units = units; } in
             { cat_intf_map =
                 List.fold_left
                   (fun m intf ->
                      let prev = find_default [] intf m in
                        DM.add intf (lib :: prev) m)
                   cat.cat_intf_map
                   cmis}
         with Failure msg -> eprintf "%s\n%!" msg; cat
           | e -> prerr_endline (Printexc.to_string e); cat)
      empty_catalog
      cmxs_files

let display_catalog catalog =
  prerr_endline (String.make 78 '=');
  DM.iter
    (fun (cminame, cmidigest) l ->
       eprintf "%s %s\n" (Digest.to_hex cmidigest) cminame;
       List.iter (fun lib -> eprintf "  %s\n" lib.lib_filename) l)
    catalog.cat_intf_map;
  prerr_endline (String.make 78 '=')

let display_solution sol =
  eprintf "Will load these libraries:\n";
  List.iter (fun lib -> eprintf "  %s\n" lib.lib_filename) sol.st_libs
