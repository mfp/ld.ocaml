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
  gencode (Ld_util.extract_units Sys.argv.(1))
