
open Printf
open Ld_util
open Ld_known_modules

let cache_file = ".ld.ocaml.cache"

let () =
  debug :=
    try
      int_of_string (Sys.getenv "LD_OCAML_VERBOSE")
    with Not_found -> 0
      | _ -> 1

let t0 = Unix.gettimeofday ()

let sys_catalog =
  try
    load_catalog (Filename.concat (Sys.getenv "HOME") cache_file)
  with
    | Not_found (* no HOME *) -> build_catalog default_dirs
    | Sys_error _ ->
        let cat = build_catalog default_dirs in
          save_catalog cat (Filename.concat (Sys.getenv "HOME") cache_file);
          cat

let catalog =
  try
    let extra = Str.split (Str.regexp ":") (Sys.getenv "LD_OCAML_LIBRARY_PATH") in
      merge_catalogs
        [ sys_catalog;
          build_catalog
            (List.filter (fun d -> not (List.mem d default_dirs)) extra) ]
  with Not_found -> (* no extra paths *) sys_catalog

let () = if !debug >= 2 then display_catalog catalog

let () =
  let state = state_of_known_modules ~known_interfaces ~known_implementations in
  let rec extract_cmxs_args = function
    | f :: tl when is_cmxs f -> f :: extract_cmxs_args tl
    | _ -> [] in
  let cmxs = extract_cmxs_args (List.tl (Array.to_list (Sys.argv))) in
    (* TODO: should rewrite Sys.argv instead, need to extract cmxs args in C
     * code before initializing the caml runtime *)
    Arg.current := 1 + List.length cmxs;
    if !debug >= 1 then
      eprintf "Built DLL catalog in %5.3fs.\n" (Unix.gettimeofday () -. t0);
    let sol = resolve catalog state cmxs in
      if !debug >= 1 then begin
        eprintf "Loading:\n";
        List.iter (eprintf "  %s\n") cmxs;
        display_solution sol;
        flush stderr;
      end;
      load_deps sol;
      List.iter (do_load sol) cmxs
