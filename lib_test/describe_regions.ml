open Ec2

let region = "us-west-2"

let _ = 
  let describe_regs = Lwt_main.run (Regions.describe ~region) in
  let print_resp {name; endpoint} = print_endline (Printf.sprintf "%s: %s" name endpt)
  List.map print_resp describe_regs

