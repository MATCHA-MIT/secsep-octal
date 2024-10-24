let read_file (filename: string) : string =
  let channel = open_in filename in
  try
    let content = really_input_string channel (in_channel_length channel) in
    close_in channel;
    content
  with e ->
    close_in_noerr channel;
    raise e

let get_output_filename (prog_name: string) (out_dir: string) (suffix: string) : string =
  Printf.sprintf "./%s/%s.%s" out_dir prog_name suffix
