(** Printing on console tty's  *)
open Core.Std
module Deferred : module type of Async.Std.Deferred
module Writer : module type of Async.Std.Writer

(** Handling of ansi codes. *)
module Ansi : sig
  val kill_line : unit -> unit
  val bell : unit -> unit
  val home_cursor : unit -> unit
  val save_cursor : unit -> unit
  val unsave_cursor : unit -> unit

  type color = [
  | `Black
  | `Red
  | `Green
  | `Yellow
  | `Blue
  | `Magenta
  | `Cyan
  | `White
  ]

  type attr = [
  | `Bright
  | `Dim
  | `Underscore
  | `Reverse
  | color
  | `Bg of color
  ]

  val printf  : attr list -> ('a, out_channel, unit) format -> 'a
  val eprintf : attr list -> ('a, out_channel, unit) format -> 'a

  val output_string : attr list -> out_channel -> string -> unit
  val output : attr list -> out_channel -> string -> int -> int -> unit

  (* Create string with embedded formatting codes *)
  val string_with_attr : attr list -> string -> string

end

val is_color_tty : unit -> bool

(** The width in characters of the current output. Returns [None] if stdout is
    not connected to a tty.*)
val width : unit -> [ `Cols of int | `Not_a_tty | `Not_available ]

(** print a list in a columnize way (like the output of ls) *)
val print_list : out_channel -> (string * Ansi.attr list) list -> unit

module Log : sig
  module Output : sig
    (** returns a [Log.Output.t] given optional styles (i.e. values of type [Ansi.t list])
        for each of the [`Debug], [`Info], and [`Error] log levels. The default styling is
        to display debug messages in yellow, error messages in red, and info messages
        without any additional styling.

        [create] doesn't take a [format] argument because colorized output should be read
        by humans.
    *)
    val create :
      ?debug:Ansi.attr list
      -> ?info:Ansi.attr list
      -> ?error:Ansi.attr list
      -> Async.Std.Writer.t
      -> Async.Std.Log.Message.t Queue.t
      -> unit Deferred.t
  end

  module Blocking : sig
    module Output : sig
      (** as [Output.create] but for use with non-async logs *)
      val create :
        ?debug:Ansi.attr list
        -> ?info:Ansi.attr list
        -> ?error:Ansi.attr list
        -> Out_channel.t
        -> Async.Std.Log.Message.t
        -> unit
    end
  end
end
