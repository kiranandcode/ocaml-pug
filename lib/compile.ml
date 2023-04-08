
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

let rec compile_inner : bool -> Ast.t -> _ H.elt =
  fun pre ->
  function
  | Ast.Nested ({value; classes; attributes}, children) ->
    H.Unsafe.node value
      ~a:(make_attributes ~classes ~attributes)
      (List.map (compile_inner String.(pre || value = "code" || value = "pre")) children)
  | Ast.Text txt -> H.txt (if pre then txt ^ "\n" else String.trim txt)

let compile ast = compile false ast

