
module H = Tyxml.Html

let make_attributes ~attributes ~classes =
  let attributes = List.map (function
      | (attr, Some vl) -> H.Unsafe.string_attrib attr vl
      | (attr, None) -> H.Unsafe.string_attrib attr attr
    ) attributes in
  match attributes, classes with
  | [], [] -> []
  | [], classes -> [H.a_class classes]
  | attributes, [] -> attributes
  | _, _ -> attributes @ [H.a_class classes]

let rec compile: Ast.t -> _ H.elt = function
  | Ast.Nested ({value; classes; attributes}, children) ->
    H.Unsafe.node value
      ~a:(make_attributes ~classes ~attributes)
      (List.map compile children)
  | Ast.Text txt -> H.txt (String.trim txt)


