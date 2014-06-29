module Field = struct
	     
  (* Turns ("Field","value") into "Field=value" *)
  let to_string (f,v) = Printf.sprintf "%s=%s" f v

  (* Turns ["Action","act";"Version","2014"] into "Action=act&Version=2014" *)
  let query_string params = 
    List.map to_string params |> String.concat "&"

  (* Turns "RegionNames.%i" ["eu-west-1";"us-east-1"] into
     [("RegionNames.1","eu-west-1"); ("RegionNames.2","us-east-1")] *)
  let number_fields pattern values =
    let rec number values n = match values with
      | [] -> []
      | (v::vs) -> Printf.sprintf pattern n :: (number vs (n+1)) in
    let names = number values 1 in
    List.combine names values
		 
  let number_fields' pattern values = 
    let rec number values n = match values with
      | [] -> []
      | (v::vs) -> pattern n :: (number vs (n+1)) in
    let names = number values 1 in
    List.combine names values

  let format_filters fs =
    let rec number names values i = match names with
      | [] -> []
      | n::ns -> match values with
		 | [] -> []
		 | (v::vs) -> number_fields' (Printf.sprintf "Filter.%i.Value.%i" i) v
			      :: number ns vs (i+1) in
    let f_names = List.map fst fs in
    let f_vals = List.map snd fs in
    let num_vals = List.concat (number f_names f_vals 1 ) in
    let num_names = number_fields "Filter.%i.Name" f_names in
    List.rev_append num_vals num_names
		    
  let add_param ?value name params = match value with 
    | Some v -> (name, v)::params 
    | None -> params
	     
end

module API = struct

  open Signature4

  let ec2 = { service = "ec2";
	      version = "2014-05-01"; }
	      
  let handle_response action fn (envelope,body) = 
    let open Monad in
    lwt body = Cohttp_lwt_body.to_string body in
    let (_,body) = Ezxmlm.from_string body in
    try_lwt
      let body = Ezxmlm.member (action^"Response") body in 
      let r = fn body in
      Lwt.return (Monad.response r)    
      with exn ->
	let awserr_of_str x = 
	  let open Ezxmlm in
	  Monad.({ code = member "Code" x |> data_to_string;
		   msg = member "Message" x |> data_to_string ; }) in	      
	let body = Ezxmlm.member "Response" body in
	let errs = List.map awserr_of_str (Ezxmlm.member "Errors" body 
					   |> Ezxmlm.members "Error") in
	Lwt.return Monad.(error (Generic (envelope, errs)))
		   
  let lwt_req {Monad.api; body; headers; meth; uri} =
    Cohttp_lwt_unix.Client.call ~headers ~body ~chunked:false meth uri
				
  let request action fn req = 
    lwt resp = lwt_req req in
    handle_response action fn resp
		      
  let verb meth action fn ?region ~params = 
    let region = match region with
      | Some r -> r
      | None -> try Unix.getenv "REGION" with exn -> "us-east-1" in
    let uri = Uri.of_string (Printf.sprintf "https://ec2.%s.amazonaws.com/" region) in
    let body = Field.query_string (List.rev_append ["Action",action; "Version", ec2.version] 
						   params) in
    let headers = realize_headers meth uri body ec2 region in
    request action fn Monad.({ api = ec2;
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
    let params = [("InstanceID", InstanceID.to_string id); ("Name", name)] in
    let params = Field.add_param ?value:description "Description" params in
    API.get "CreateImage" ~params create_img_of_string ?region
 
  let deregister_image id ?region () =
    let params = [("ImageId", ImageID.to_string id)] in
    API.get "DeregisterImage" ~params dereg_img_of_string ?region

  let register_image ~name ?img_path ?desc ?arch ?region () =
    let params = [("Name", name)] in
    let params = Field.add_param ?value:img_path "ImageLocation" params 
		 |> Field.add_param ?value:desc "Description"
		 |> Field.add_param ?value:arch "Architecture" in
    API.get "RegisterImage" ~params reg_img_of_string ?region
 
end 

module EBS = struct

  let create_snapshot id ?description ?region () =
    let params = [("VolumeId", VolumeID.to_string id)] in
    let params = Field.add_param ?value:description "Description" params in
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

  let run ?(min=1) ?(max=1) ?(instance="m1.small") ?zone ?kernel id ?region () =
    let params = Field.add_param ?value:zone "Placement.AvailabilityZone" [] in
    let params = Field.add_param ?value:kernel "KernelId" params in
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

module KeyPairs = struct

  let describe ?(names=[]) ?(filters=[]) ?region () = 
    let params = Field.number_fields "KeyName.%i" names in
    let params = List.rev_append params
				 (Field.format_filters filters) in
    API.get "DescribeKeyPairs" ~params desc_keys_of_string ?region

end

module Regions = struct
  
  (*?filters = 
[("endpoint",["*ap*"])] -> "Filter.1.Name=endpoint&Filter.1.Value.1=*ap*"*)

  (* Numbering the filters might be unduly complicated. *)
  let describe ?(regions=[]) ?(filters=[]) ?region () =
    let regions = Field.number_fields "RegionName.%i" (List.map string_of_region regions) in
    let filters = Field.format_filters filters in
    let params = List.rev_append regions filters in
    API.get "DescribeRegions" ~params ?region desc_regions_of_string
 	    
end
