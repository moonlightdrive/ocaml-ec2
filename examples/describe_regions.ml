open EC2
open EC2_t

let _ = 
  let filters = [("endpoint",["*ap*";"*west*"])] in
  let describe_regs = Lwt_main.run (Monad.run (Regions.describe ~filters () )) in
  let print_resp {Region.name; endpoint} = print_endline (Printf.sprintf "%s: %s" 
								  (string_of_region name) 
								  endpoint) in 
  List.map print_resp describe_regs
