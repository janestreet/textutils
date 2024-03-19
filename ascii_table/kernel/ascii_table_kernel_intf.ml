open! Core
open! Import

(* Note: For UTF-8 tables, this module makes a naive assumption that each grapheme cluster
   in the text is comprised of only one Unicode scalar value. If this is not the case, the
   width calculations will be incorrect. *)

module type Ascii_table_kernel = sig
  module Attr = Attr
  module Align = Column.Align

  module Display : sig
    type t

    (** Default--cells can be multi-line. *)
    val short_box : t

    (** Puts --- between entries. *)
    val tall_box : t

    (** Trails off with ... if necessary. *)
    val line : t

    (** No lines. *)
    val blank : t

    (** Draw lines only under column titles. *)
    val column_titles : t
  end

  module Column : module type of struct
    include Column
  end
  with module Private := Column.Private

  module Screen : sig
    (** A [Screen.t] represents a table after all of the layout calculations have been done.
    *)
    type t

    val render
      :  t
      -> bars:[ `Ascii | `Unicode ]
      -> output:(Attr.t list -> Buffer.t -> unit)
      -> close:(Buffer.t -> 'a)
      -> 'a

    (** Given a way to annotate strings with attributes, a [t] can be output to a string. *)
    val to_string
      :  t
      -> bars:[ `Ascii | `Unicode ]
      -> string_with_attr:(Attr.t list -> string -> string)
      -> string
  end

  val draw
    :  ?display:Display.t (** Default: short_box *)
    -> ?spacing:int (** Default: 1 *)
    -> ?limit_width_to:int (** defaults to 90 characters *)
    -> ?header_attr:Attr.t list
    -> ?display_empty_rows:bool (** Default: false *)
    -> prefer_split_on_spaces:bool
    -> 'row Column.t list
    -> 'row list
    -> Screen.t option

  val to_string_noattr
    :  ?display:Display.t (** Default: short_box *)
    -> ?spacing:int (** Default: 1 *)
    -> ?limit_width_to:int (** defaults to 90 characters *)
    -> ?display_empty_rows:bool (** Default: false *)
    -> ?prefer_split_on_spaces:bool (** Default: false  *)
    -> 'row Column.t list
    -> 'row list
    -> bars:[ `Ascii | `Unicode ]
    -> string

  module Table_char : sig
    type t =
      { ascii : char
      ; utf8 : string
      }

    val connect : ?top:unit -> ?bottom:unit -> ?left:unit -> ?right:unit -> unit -> t
  end

  module Private : sig
    module Utf8_text_chunks = Utf8_text_chunks
  end
  [@@alert
    ascii_table_kernel_private
      "This module is intended only for tests of Ascii_table_kernel."]
end
