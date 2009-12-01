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
