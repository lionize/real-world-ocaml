open Core.Std

(*
 *
 * Usage:
   * printf "%s\n"
   *  (render_table
   *    ["language"; "architect"; "first_release"]
   *    [ ["Lisp"  ; "John McCarthy" ; "1958"] ;
   *      ["C"     ; "Dennis Ritchie"; "1969"] ;
   *      ["ML"    ; "Robin Milner"  ; "1973"] ;
   *      ["OCaml" ; "Xavier Leroy"  ; "1996"] ;
   *    ]
   *  );;
 *
 *)

let max_widths header rows =
  let lengths l = List.map ~f:String.length l in
  List.fold rows
    ~init:(lengths header)
    ~f:(fun acc row ->
      List.map2_exn ~f:Int.max acc (lengths row))

let render_separator widths =
  let pieces = List.map widths
    ~f:(fun w -> String.make (w + 2) '-')
  in
  "|" ^ String.concat ~sep:"+" pieces ^ "|"

let pad s length =
  " " ^ s ^ String.make (length - String.length s + 1) ' '

let render_row row widths =
  let padded = List.map2_exn row widths ~f:pad in
  "|" ^ String.concat ~sep:"|" padded ^ "|"

let render_table header rows =
  let widths = max_widths header rows in
  String.concat ~sep:"\n"
    (render_row header widths
     :: render_separator widths
     :: List.map rows ~f:(fun row -> render_row row widths)
    )

render_table ["col1"; "col2"; "col3"] [["info1-1"; "info1-2"; "info1-3"]; ["info2-1"; "info2-2"; "info2-3"]]
