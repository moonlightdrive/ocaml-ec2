open EC2

(* Launches a m1.small instance from a manifest *)

let bucket = "jymirage"
let my_kernel = "mirage.img.manifest.xml"
let region = "us-west-2"

let t = 
  let image = Printf.sprintf "%s/%s" bucket my_kernel in
  Monad.bind (AMI.register_image "my_new_ami" ~image ~region ()) (fun id -> Instances.run id ~region ())

let _ = Lwt_main.run (Monad.run t)
