open EC2_t

let region = "us-west-2"

let _ = 
  let describe_regs = Lwt_main.run (EC2.Regions.describe ~region) in
  let print_resp {name; endpoint} = print_endline (Printf.sprintf "%s: %s" name endpoint) in 
  List.map print_resp describe_regs
