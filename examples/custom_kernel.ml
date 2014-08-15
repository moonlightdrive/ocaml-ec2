open EC2
open EC2_t
open Lwt

let image = "my.img"
let my_key = "myprivatekey.pem"
let my_cert = "mycert.pem"
let region = US_WEST_2
let my_bucket = "mybucket"

let start_instance img_file key cert bucket () =
  print_endline "Bundling image...";
  let (manifest_path, part_paths) as files = Bundle.bundle_img ~key ~cert img_file |> Lwt_main.run in
  print_endline "Uploading bundle (this may take a while)...";
  ignore @@ List.map Lwt_main.run @@ Bundle.upload ~bucket files ~region;
  print_endline "Registering AMI...";
  let img_path = 
    let kernel =  manifest_path in
    Printf.sprintf "%s%s" bucket kernel in
  Monad.bind (AMI.register_image ~name:"ocaml-ec2" ~img_path ~region ())
	     (fun id -> print_endline @@ 
			  Printf.sprintf "Launching instance %s" (ImageID.to_string id);
			Instances.run id ~region ()) 

let _ = Lwt_main.run @@ 
	  Monad.run @@ start_instance image my_key my_cert my_bucket ()

