
let run file =
  In_channel.with_open_bin file (fun ic ->
      let _text = (In_channel.input_all ic) in
      let res = Pug.parse _text in
      let pp = Tyxml_html.pp_elt ~indent:true () in
      match res with
      | Error s -> failwith s
      | Ok res ->
        List.iter (Format.printf "%a@." pp)
          ((List.map Pug.compile res));
    )

open Cmdliner

let file =
  let info =
    Arg.info ~docv:"FILE" ~doc:"$(docv) is the file to load" [] in
  Arg.required (Arg.pos 0 Arg.(some file) None info)


let _ =
  let info = Cmd.info "pug"
      ~doc:"OCaml pug to html compiler" in
  Cmd.eval @@ Cmd.v info Term.(const run $ file)
