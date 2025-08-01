type statistics_t = {
  mutable time: float;
  mutable smt_queries: int;
  mutable smt_queries_time: float;
}

let stat: statistics_t = {
  time = 0.0;
  smt_queries = 0;
  smt_queries_time = 0.0;
}

let statistics_to_file (filename: string) =
  let channel = open_out filename in
  Printf.fprintf
    channel
    "{\n  \"time\": %f,\n  \"smt_queries\": %d,\n  \"smt_queries_time\": %f\n}\n"
    stat.time
    stat.smt_queries
    stat.smt_queries_time;
  close_out channel
