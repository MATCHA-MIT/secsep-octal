let read_file (filename: string) : string =
  let channel = open_in filename in
  try
    let content = really_input_string channel (in_channel_length channel) in
    close_in channel;
    content
  with e ->
    close_in_noerr channel;
    raise e

let get_single_infer_filename (s: string) : string =
  Printf.sprintf "./out/%s.single_infer" s

let get_range_infer_filename (s: string) : string =
  Printf.sprintf "./out/%s.range_infer" s

let get_taint_infer_filename (s: string) : string =
  Printf.sprintf "./out/%s.taint_infer" s
