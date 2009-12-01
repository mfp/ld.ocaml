
open Printf
open Ld_util

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

