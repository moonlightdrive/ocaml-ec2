open EC2
open EC2_t

let image = "my.img"
let my_key = "myprivatekey.pem"
let my_cert = "mycert.pem"
let region = US_WEST_2
let my_bucket = "mybucket"

let register_ami img_file key cert bucket () =
  print_endline "Bundling image...";
  let (manifest_path, part_paths) as files = Bundle.bundle_img ~key ~cert img_file |> Lwt_main.run in
  print_endline "Uploading bundle (this may take a while)...";
  ignore @@ List.map Lwt_main.run @@ Bundle.upload ~bucket files ~region;
  print_endline "Registering AMI...";
  let img_path = 
    let kernel =  manifest_path in
    Printf.sprintf "%s%s" bucket kernel in
  AMI.register_image ~name:"ocaml-ec2" ~img_path ~region ()

let launch_instance img key cert bucket =
  Monad.bind 
    (register_ami img key cert bucket ())
    (fun id -> Instances.run id ~region ())

(* Register an AMI without launching *)
let _ = 
  let ami = Lwt_main.run @@ Monad.run @@ register_ami image my_key my_cert my_bucket () in
  print_endline @@ 
    Printf.sprintf "Registered AMI.\nLaunch this instance with `Instances.run \"%s\" ()`"
		   (ImageID.to_string ami)
    
(* Registers AMI & launches an instance *)
(*
let _ = 
  let running = Lwt_main.run @@ Monad.run @@ launch_instance image my_key my_cert my_bucket in
  running.instances |>
    List.map (fun (i:running_instance) -> print_endline @@ Printf.sprintf "Launched instance %s" i.id) 
*)
 
  
