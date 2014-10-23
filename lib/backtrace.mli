(*
 * Copyright (C) 2006-2009 Citrix Systems Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)

val is_important: exn -> unit
(** Declare that the backtrace is important for debugging and should be preserved.
    The default is to throw away backtraces for speed. *)

val get: exn -> string list
(** Get a copy of the backtrace associated with [exn] *)

val add: exn -> string list -> unit
(** Associate additional backtrace with [exn] *)

val reraise: exn -> exn -> 'a
(** [reraise old new] associates the backtrace of [old] with [new]
    and throws [new] *)

val remove: exn -> string list
(** Get a backtrace associated with [exn] and remove it from the tables *)
