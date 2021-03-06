(* Copyright (c) 2009 Mauricio Fernández <mfp@acm.org> *)

(** OCaml dynamic loader utility lib *)

(** Type of DLL catalogs *)
type catalog

(** Solution to a dependency problem  *)
type state

(** Information about a library *)
type lib = { lib_filename : string; lib_units : Ld_header.dynunit list; }

(** Debug level: 0 corresponds to no debug, higher levels will cause debug
  * info to be printed to stderr. *)
val debug : int ref

(** Default search path for .cmxs libs. *)
val default_dirs : string list

(** Scan the given dirs for .cmxs files and build a DLL catalog. *)
val build_catalog : string list -> catalog

(** Load catalog from the given file. *)
val load_catalog : string -> catalog

(** Save catalog to the specified file. *)
val save_catalog : catalog -> string -> unit

(** Combine multiple catalogs into one. *)
val merge_catalogs : catalog list -> catalog

(** [resolve catalog init_state files] gives a final state representing the
  * libs that need to be loaded before loading the [files] .cmxs *)
val resolve : catalog -> state -> string list -> state

(** [unresolved_modules state] returns a list of unresolved dependencies in
  * [state] *)
val unresolved_modules : state -> (string * Digest.t) list

(** [do_load state file] uses Dynlink to load the given .cmxs file. *)
val do_load : state -> string -> unit

(** [load_deps state] loads the required libraries to reach [state]. *)
val load_deps : state -> unit

(** [run catalog init_state files] finds the final state needed to load the
  * cmxs [files], loads the required libs and finally the cmxs files
  * themselves. *)
val run : catalog -> state -> string list -> unit

(** [extract_units file] extract information about the units defined in
  * [file]. *)
val extract_units : string -> Ld_header.dynunit list

(** Print the catalog to stderr. *)
val display_catalog : catalog -> unit

(** [display_solution state] prints to stderr the libraries that will be
* loaded to reach the given [state].  *)
val display_solution : state -> unit

(** Creates an initial state from list of [name, digest (hexadecimal)] pairs
  * for the known CMIs and CMXs. *)
val state_of_known_modules :
  known_interfaces:(string * string) list ->
  known_implementations:(string * string) list -> state

(** [is_cmxs s] returns true if [s] ends with ".cmxs". *)
val is_cmxs : string -> bool
