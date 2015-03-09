let number_nonblank = ref false
let number = ref false
let squeeze_blank = ref false
let show_nonprinting = ref false
let show_ends = ref false
let show_tabs = ref false
let enable_output = ref true
let filenames = ref []

let spec =
  [("-b",
    Arg.Set number_nonblank,
    "Number the non-blank output lines, starting at 1."); ("-e",
    Arg.Unit (fun () ->
      show_nonprinting := true;
      show_ends := true),
    "Display non-printing characters and display '$' at the end of each line.");
   ("-n",
    Arg.Set number,
    "Number the output lines, starting at 1.");
   ("-s",
    Arg.Set squeeze_blank,
    "Squeeze multiple adjacent empty lines, causing the output to be single spaced.");
   ("-t",
    Arg.Unit (fun () ->
      show_nonprinting := true;
      show_tabs := true),
    "Display non-printing characters, and display tab characters as `^I'.");
   ("-u", Arg.Clear enable_output, "Disable output buffering.");
   ("-v",
    Arg.Set show_nonprinting,
    "Display non-print-ing characters so they are visible.")]

let rec body blanked line channel =
  let s = input_line channel in
  if !squeeze_blank && blanked && s = "" then
    if !number_nonblank then
      body true line channel
    else
      body true (line + 1) channel
  else
    if not !number_nonblank && !number || !number_nonblank && s <> "" then
      Printf.printf "%6d " line;
    String.iter (fun c ->
      if !show_tabs && c = '\t' then
        print_string "^I"
      else if !show_nonprinting && Char.code c land 0x80 = 1 then
        Printf.printf "M-%d" @@ Char.code c land 0x7F
      else
        print_char c) s;
    if !show_ends then print_endline "$"
    else print_newline ();
    if !number_nonblank && s = "" then
      body (s = "") line channel
    else
      body (s = "") (line + 1) channel

let () =
  Arg.parse spec
    (fun s -> filenames := s :: !filenames)
    "Usage: cat [-benstuv] [-help] filename ...";
  if !enable_output then begin
  List.iter
    (fun filename ->
      let channel = open_in filename in
      try body false 1 channel with End_of_file -> ();
      close_in channel)
    (List.rev !filenames)
  end
