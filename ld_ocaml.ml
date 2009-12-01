
open Printf
open Ld_util

let catalog = build_catalog ()

let () =
  if !debug then begin
    print_endline (String.make 78 '=');
    DM.iter
      (fun (cmxname, cmxdigest) l ->
         printf "%s %s\n" (Digest.to_hex cmxdigest) cmxname;
         List.iter (fun lib -> printf "  %s\n" lib.lib_filename) l)
      catalog.cat_intf_map;
    print_endline (String.make 78 '=');
  end

