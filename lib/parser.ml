open Angstrom
module StringMap = Map.Make (String)
open Ast

let is_alpha c = ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z')
let is_digit c = ('0' <= c && c <= '9')
let is_alnum c = is_alpha c || is_digit c
let is_dash c = (c = '_' || c = '-')
let is_attr_char c = is_alnum c || is_dash c
let is_whitespace c= c = ' ' || c = '\t' || c = '\n'

let ws = take_while is_whitespace

let html_tag =
  satisfy is_alpha >>= fun c ->
  take_while is_alnum >>= fun cs ->
  return (String.make 1 c ^ cs)

let html_attribute =
  satisfy is_alpha >>= fun c ->
  take_while is_attr_char >>= fun cs ->
  return (String.make 1 c ^ cs)

let int = take_while1 is_digit

let any = take_till (Fun.negate @@ Char.equal '\n')

let quoted_string =
  let* _ = char '"' in
  let* text = (many @@ 
               ((let* _ = char '\\' in
                 let* c = any_char in
                 return (String.make 1 c)) <|>
                take_while1 (Fun.negate @@ fun c -> c = '"' || c = '\\'))) <|>
              return [] in
  let* _ = char '"' in
  return (String.concat "" text)

let single_quoted_string =
  let* _ = char '\'' in
  let* text = (many @@ 
               ((let* _ = char '\\' in
                 let* c = any_char in
                 return (String.make 1 c)) <|>
                take_while1 (Fun.negate @@ fun c -> c = '\'' || c = '\\'))) <|>
              return [] in
  let* _ = char '\'' in
  return (String.concat "" text)

let quoted_text = quoted_string <|> single_quoted_string

let dotted_property =
  let* _ = char '.' in
  let* class_name = html_attribute in
  return class_name

let attribute_eq =
  let* attr = html_attribute in
  let* _ = ws in
  let* _ = char '=' in
  let* _ = ws in
  let* value = html_attribute <|> quoted_text <|> int in
  return (attr, Some value)

let plain_attribute =
  let* attr = html_attribute in
  return (attr, None)

let attributes =
  let* _ = char '(' in
  let* attributes =
    sep_by (ws *> char ',') (ws *> (attribute_eq <|> plain_attribute)) in
  let* _ =
    (ws *> char ')') in
  return attributes

let tag =
  let* tag_value = html_tag in
  let* attributes = many attributes in
  let* classes = many dotted_property in
  let attributes =
    List.flatten attributes
    |> List.fold_left (fun map (key,vl) ->
        StringMap.update key (fun old_vl ->
            Some (vl :: (Option.value ~default:[] old_vl))
          ) map
      ) StringMap.empty in
  let attr_classes = StringMap.find_opt "class" attributes
                     |> Option.value ~default:[]
                     |> List.filter_map Fun.id in
  let classes = attr_classes @ classes in
  let attributes = StringMap.remove "class" attributes in
  return {
    value=tag_value;
    classes;
    attributes=
      (StringMap.to_seq attributes)
      |> Seq.map (fun (key,vl) ->
          let values = List.filter_map Fun.id vl in
          match values with
          | [] -> key, None
          | _ -> key, Some (String.concat " " values)
        )
      |> List.of_seq;
  }

let nested_tags t indent =
  let* t = t in
  let* tag = tag in
  let* _ = char ':' in
  let* _ = take_while (Char.equal ' ') in
  let* subtag = t indent in
  return (Nested (tag, [subtag]))

let enforce_whitespace indent =
  (let* ws = take_while (Char.equal ' ') in
  if (String.length ws <= indent)
  then fail "not a subcomponent"
  else return (String.length ws))

let tagged_block_children t indent =
  let* _ = char '\n' in
  let* children = many (
      let* indent = enforce_whitespace indent in
      let* res = t indent in
      let* _ = (char '\n' <|> return ' ') in
      return res
    ) in
  return ( children)

let tagged_block t indent =
  let* t = t in
  let* tag = tag in
  let* contents = take_while (Fun.negate @@ Char.equal '\n') in
  let* children =
    (tagged_block_children t indent <|> return []) in
  let children = if String.for_all is_whitespace contents
    then children else Text contents :: children in
  return (Nested (tag, children))

let plain_text =
  let* _ = char '|' in
  let* contents = take_while (Fun.negate @@ Char.equal '\n') in
  return (Text contents)


let blank_line t indent =
  let* t = t in
  let* _ = take_while (fun c -> c = ' ') *> char '\n' in
  t indent


let t = fix (fun (t: (int -> t Angstrom.t) Angstrom.t) ->
    return (fun indent ->
        plain_text <|>
        nested_tags t indent <|>
        tagged_block t indent <|>
        blank_line t indent)
  )


let blank_line =
  let* _ = take_while (fun c -> c = ' ') *> char '\n' in
  return []

let t indent =
  let* t = t in
  t indent

let t =
  let* res = t 0 in
  return [res]

let parse =
  let* res = many (blank_line <|> t) in
  return (List.flatten res)


let parse = Angstrom.parse_string ~consume:Consume.All parse


