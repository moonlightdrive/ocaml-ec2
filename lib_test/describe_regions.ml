open EC2
open EC2_t

let _ = 
  let describe_regs = Lwt_main.run (Monad.run (Regions.describe () )) in
  let print_resp {name; endpoint} = print_endline (Printf.sprintf "%s: %s" 
								  (string_of_region name) 
								  endpoint) in 
  List.map print_resp describe_regs
