
open Printf
open Ld_util
open Ld_known_modules

let catalog = build_catalog ("." :: default_dirs)

let () =
  debug :=
    try
      int_of_string (Sys.getenv "LD_OCAML_VERBOSE")
    with Not_found -> 0
      | _ -> 1

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
    let sol = resolve catalog state cmxs in
      if !debug >= 1 then begin
        eprintf "Loading:\n";
        List.iter (eprintf "  %s\n") cmxs;
        display_solution sol;
        flush stderr;
      end;
      load_deps sol;
      List.iter do_load cmxs
