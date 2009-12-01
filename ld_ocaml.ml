
open Printf
open Ld_util
open Ld_known_modules

let catalog = build_catalog ()

let () =
  if !debug then begin
    prerr_endline (String.make 78 '=');
    DM.iter
      (fun (cmxname, cmxdigest) l ->
         eprintf "%s %s\n" (Digest.to_hex cmxdigest) cmxname;
         List.iter (fun lib -> eprintf "  %s\n" lib.lib_filename) l)
      catalog.cat_intf_map;
    prerr_endline (String.make 78 '=');
  end

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
      if !debug then begin
        eprintf "Loading:\n";
        List.iter (eprintf "  %s\n") cmxs;
        eprintf "Will load these libraries:\n";
        List.iter (fun lib -> eprintf "  %s\n" lib.lib_filename) sol.st_libs
      end
