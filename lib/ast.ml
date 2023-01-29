type tag = {
  value: string;
  classes: string list;
  attributes: (string * string option) list
}
[@@deriving show]
type t =
  | Nested of tag * t list
  | Text of string
[@@deriving show]
