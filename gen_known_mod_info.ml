(* Copyright (c) 2009 Mauricio Fernández <mfp@acm.org> *)

open Ld_header
open Printf

let gencode units =
  let dump f =
    List.iter
      (fun u ->
         List.iter
           (fun (name, digest) -> printf "  %S, %S;\n" name (Digest.to_hex digest))
           (f u))
      units
  in
    printf "let known_interfaces = [\n";
    dump (fun u -> u.imports_cmi);
    printf "]\n\nlet known_implementations = [\n";
    dump (fun u -> u.imports_cmx);
    printf "]\n\n"

let () =
  (* let file = dll_filename Sys.argv.(1) in *)
  (* let header_data = ld_extract_headers file in *)
  (* let header : dynheader = Marshal.from_string header_data 0 in *)
    (* if header.magic <> dyn_magic_number then *)
      (* failwith (file ^ " is not an OCaml shared library."); *)
    (* gencode header.units *)
  gencode (Ld_util.extract_units Sys.argv.(1))
