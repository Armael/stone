
let die msg =
  print_string msg;
  exit 1

let _ =
  let usage = "Text packer <input> <output.ml>\n\
    Creates an ocaml file which defines a string text,\n\
    containing the (properly escaped) content of input\n" in
  if Array.length Sys.argv <> 3 then
    die usage;

  let input = Sys.argv.(1)
  and output = Sys.argv.(2) in

  let chan_out = open_out output in
  output_string chan_out "let text =\n\"";

  let chan_in = open_in input in
  try
    while true do
      let line = input_line chan_in in
      output_string chan_out (String.escaped line);
      output_string chan_out "\n"
    done
  with End_of_file ->
    close_in chan_in;
    output_string chan_out "\"";
    close_out chan_out
  
  
    
