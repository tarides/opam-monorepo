(**************************************************************************)
(*                                                                        *)
(*    Copyright 2017-2019 OCamlPro                                        *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

(** This module polls various aspects of the host, to define the [arch], [os],
    etc. variables *)

val arch: unit -> string option
val os: unit -> string option
val os_distribution: unit -> string option
val os_version: unit -> string option
val os_family: unit -> string option

val variables: (OpamVariable.t * OpamTypes.variable_contents option Lazy.t) list

(** The function used internally to get our canonical names for architectures
    (returns its input lowercased if not a recognised arch). This is typically
    called on the output of [uname -m] *)
val normalise_arch: string -> string

(** The function used internally to get our canonical names for OSes (returns
    its input lowercased if not a recognised OS). This is typically called on
    the output of [uname -s] *)
val normalise_os: string -> string

(* Number of cores *)
val cores: unit -> int

(** Returns a string containing arch, os, os-distribution & os-version values,
    unknown if they are not available *)
val to_string: unit -> string
