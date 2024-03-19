open! Core

(** [of_utf8 utf8 ~width ~prefer_split_on_spaces] splits [utf8] into chunks no wider than
    [width] characters. [chunks_of_utf8] always returns at least one chunk, which may be
    empty.

    If [prefer_split_on_spaces = true] and such a space exists, [utf8] will be split on
    the last U+0020 SPACE before the chunk becomes too wide. Otherwise, the split happens
    exactly at [width] characters. *)
val of_utf8
  :  String.Utf8.t
  -> width:int
  -> prefer_split_on_spaces:bool
  -> String.Utf8.t list
