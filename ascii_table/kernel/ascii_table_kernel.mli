open! Core_kernel

module Attr : sig
  (* This is equal to Console.Ansi.attr, but we don't expose that type equivalence to
     avoid depending on core *)
  type color =
    [ `Black
    | `Red
    | `Green
    | `Yellow
    | `Blue
    | `Magenta
    | `Cyan
    | `White
    ]

  type t =
    [ `Bright
    | `Dim
    | `Underscore
    | `Reverse
    | color
    | `Bg of color
    ]
end

module Align : sig
  type t =
    | Left
    | Right
    | Center
end

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

module Column : sig
  type 'a t

  (** creates a column given the header and the to-string function *)
  val create
    :  ?align:Align.t (* Default: left *)
    -> ?min_width:int
    -> ?max_width:int
    -> ?show:[ `Yes | `No | `If_not_empty ] (* Default: `Yes *)
    -> string
    -> ('a -> string)
    -> 'a t

  (** like create, except that the to_string function must provide a list of
      attributes. *)
  val create_attr
    :  ?align:Align.t (* Default: left *)
    -> ?min_width:int
    -> ?max_width:int
    -> ?show:[ `Yes | `No | `If_not_empty ] (* Default: `Yes *)
    -> string
    -> ('a -> Attr.t list * string)
    -> 'a t

  val header : 'a t -> string
  val to_data : 'a t -> 'a -> Attr.t list * string list
end

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
  :  ?display:Display.t (* Default: short_box *)
  -> ?spacing:int (* Default: 1 *)
  -> ?limit_width_to:int (* defaults to 90 characters *)
  -> ?header_attr:Attr.t list
  -> ?display_empty_rows:bool (* Default: false *)
  -> 'row Column.t list
  -> 'row list
  -> Screen.t option

module Table_char : sig
  type t =
    { ascii : char
    ; utf8 : string
    }

  val connect : ?top:unit -> ?bottom:unit -> ?left:unit -> ?right:unit -> unit -> t
end
