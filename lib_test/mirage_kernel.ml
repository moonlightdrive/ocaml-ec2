open EC2
open EC2_t

(* Launches a m1.small instance from a manifest *)

let bucket = "jymirage"
let my_kernel = "mirage.img.manifest.xml"
let region = US_WEST_2

let t  =
  let img_path = Printf.sprintf "%s/%s" bucket my_kernel in
  Monad.bind (AMI.register_image ~name:"my_new_ami" ~img_path ~region ()) 
	     (fun id -> Instances.run id ~region ())

let _ = Lwt_main.run (Monad.run t)
