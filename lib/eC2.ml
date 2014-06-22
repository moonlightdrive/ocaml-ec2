module Field = struct
	     
  (* Turns ("Field","value") into "Field=value" *)
  let to_string (f,v) = Printf.sprintf "%s=%s" f v

  (* Turns ["Action","act";"Version","2014"] into "Action=act&Version=2014" *)
  let query_string params = 
    List.map to_string params |> String.concat "&"

  (* Turns "RegionNames.%i" ["eu-west-1";"us-east-1"] into
     [("RegionNames.1","eu-west-1"); ("RegionNames.2","us-east-1")] *)
  let number_fields pattern values =
    let rec number values n = match values with (* is this efficient *)
      | [] -> []
      | (v::vs) -> Printf.sprintf pattern n :: (number vs (n+1)) in
    let names = number values 1 in
    List.combine names values

(*
  let add_param ?value name params = match value with 
    | Some value -> (name, value)::params 
    | None -> params
 *)
	     
end

module API = struct

  let service = "ec2"	
  let version = "2014-05-01"

  open Signature4

  let query uri = 
    let remove_path s = String.sub s 2 (String.length s -2) in
    remove_path (Uri.path_and_query uri)
	
  let handle_response action fn (envelope,body) = 
    let open Monad in
    try_lwt
      lwt body = Cohttp_lwt_body.to_string body in
      let (_,body) = Ezxmlm.from_string body in
      let body = Ezxmlm.member (action^"Response") body in
      let r = fn body in
      Lwt.return (Monad.response r)
      with exn ->
	Lwt.return Monad.(error (Generic envelope))

  let lwt_req {Monad.api; body; headers; meth; uri} =
    Cohttp_lwt_unix.Client.call ~headers ~body ~chunked:false meth uri

  let request action fn req = 
    lwt resp = lwt_req req in
    handle_response action fn resp

  (* TODO default region not working *)

  let verb meth action fn ?(region="us-east-1") ~params = 
    let uri = Uri.of_string (Printf.sprintf "https://ec2.%s.amazonaws.com/" region) in
    let api = { service = service; version = version; } in
    let body = Field.query_string (List.rev_append ["Action",action; "Version", api.version] 
						   params) in
    let headers = realize_headers meth uri body api region in
    request action fn Monad.({ api = api;
			       body = Cohttp_lwt_body.of_string body;
			       headers = headers;
			       meth = meth;
			       uri = uri; })
	    
  let get action fn = verb `GET action fn
 
  let post action fn = verb `POST action fn
		  
end

open EC2_t
open EC2_x

module AMI = struct

  let create_image ~name ?description id ?region () = 
    let params = [("InstanceID", id); ("Name", name)] in
    let params = match description with
      | Some description -> [("Description", description)]::params
      | None -> params in
    API.get "CreateImage" ~params create_img_of_string ?region
 
  let deregister_image id ?region () =
    let params = [("ImageId", ImageID.to_string id)] in
    API.get "DeregisterImage" ~params dereg_img_of_string ?region

  let register_image ~name ?img_path ?region () =
    let params = ("Name", name) in
    let params = match img_path with
      | Some img_path -> params::[("ImageLocation", img_path)]
      | None -> [params] in
    API.get "RegisterImage" ~params reg_img_of_string ?region
 
end 

module EBS = struct

  let create_snapshot id ?description ?region () =
    let params = ("VolumeId", VolumeID.to_string id) in
    let params = match description with
      | Some description -> params::[("Description", description)]
      | None -> [params] in
    API.get "CreateSnapshot" ~params create_snap_of_string ?region
	     
  let delete_volume id ?region () =
    let params = [("VolumeId", VolumeID.to_string id)] in
    API.post "DeleteVolume" ~params delete_vol_of_string ?region

end
	       
module Instances = struct
(*
  (* TODO implement the remaining parameters *)
  let describe_status ?ids ?(all=false) () =
    let params = ("IncludeAllInstances", string_of_bool all) in
    let params = match ids with 
      | Some ids -> params::(Field.number_fields "InstanceId.%i" ids) 
      | None -> [params] in
    API.get "DescribeInstanceStatus" ~params 
 *)
  let get_console_output id ?region () =
    let params = [("InstanceId", InstanceID.to_string id)] in
    API.get "GetConsoleOutput" ~params console_output_of_string ?region

  (* TODO this function has 1000 request parameters *)
  let run ?(min=1) ?(max=1) ?(instance="m1.small") ?zone ?kernel id ?region () =
    let params = match zone with
      | Some zone -> [("Placement.AvailabilityZone", zone)]
      | None -> [] in
    let params = match kernel with
      | Some kernel -> ("KernelId", kernel)::params
      | None -> params in
    let params = params@[("ImageId", ImageID.to_string id); 
			 ("MinCount", string_of_int min); 
			 ("MaxCount", string_of_int max)] in
    API.get "RunInstances" ~params run_instances_of_string ?region

  let start ids ?region () =
    let params = List.map InstanceID.to_string ids |>
		   Field.number_fields "InstanceId.%i" in
    API.get "StartInstances" ~params start_instances_of_string ?region

  let stop ?(force=false) ids ?region () =
    let params = ("Force", string_of_bool force) in
    let params = List.map InstanceID.to_string ids |> 
		   fun ids -> params::(Field.number_fields "InstanceId.%i" ids) in
    API.get "StopInstances" ~params stop_instances_of_string ?region

  let terminate ids ?region () = 
    let params = List.map InstanceID.to_string ids |>
		   Field.number_fields "InstanceId.%i" in
    API.get "TerminateInstances" ~params terminate_instances_of_string ?region

end 

module Regions = struct
  
  let describe ?region () =
    API.get "DescribeRegions" ~params:[] ?region desc_regions_of_string
	    
end
