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

	 		 
module Monad = struct
	       
  type request = { api: Signature4.api; 
		   body: Cohttp_lwt_body.t;
		   headers: Cohttp.Header.t;
		   meth: Cohttp.Code.meth;
		   uri: Uri.t; }

  type error = 
    | Generic of Cohttp.Response.t
    | No_response
	
  type 'a signal = 
    | Error of error
    | Response of 'a
   and 'a t = ('a signal) Lwt.t
			  
  let error e = Error e
		      
  let response r = Response r
			    
  let error_to_string = function
    | Generic err -> Printf.sprintf "HTTP Error %s\n" (Cohttp.Code.string_of_status (Cohttp.Response.status err)) (* TODO print the AWS error *)
    | No_response -> "No response"

  let bind x fn = match_lwt x with
    | Response r -> fn r
    | Error _ as e -> Lwt.return e
      
  let return r = Lwt.return (response r)	

  let fail err = Lwt.return (error err)
   
  let run x = match_lwt x with
	      | Error e -> Lwt.fail (Failure (error_to_string e))
	      | Response r -> Lwt.return r
					 
  let (>>=) = bind
		
end

module API = struct

  let service = "ec2"
		
  let version = "2014-05-01"			

  open Signature4

  let query uri = 
    let remove_path s = String.sub s 2 (String.length s -2) in
    remove_path (Uri.path_and_query uri)

  let realize_headers meth uri body_str api region =
    let open Signature in 
    let timestamp = Time.now_utc in
    let host = match Uri.host uri with
      | Some h -> h
      | None -> "ec2.us-east-1.amazonaws.com" in (* TODO don't hardcode this?? *)
    let secret = iam_secret in
    let access = iam_access in
    let cred_scope = credential_scope timestamp region api.service in
    let credentials = access^"/"^cred_scope in
    let canonical_req = canonical_request meth ~timestamp ~host ~payload:body_str () in
    let str_to_sign = str_to_sign ~timestamp ~cred_scope ~req:canonical_req in
    let signature = signature ~secret ~timestamp ~region str_to_sign api.service in
    let auth = List.map Field.to_string [ (signing_algorithm^" Credential", credentials)
					; ("SignedHeaders", signed_headers)
					; ("Signature", signature) 
					]
	       |> String.concat ", " in
    Cohttp.Header.of_list [ "Authorization", auth;
			    "Content-Type", content_type;
			    "X-Amz-Date", Time.date_time timestamp; ]
	
  let handle_response action fn (envelope,body) = 
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
	    
  let get action (fn: Ezxmlm.nodes -> 'a) = verb `GET action fn
 
  let post action (fn: Ezxmlm.nodes -> 'a) = verb `POST action fn
		  
end

open EC2_t
open EC2_x

module AMI = struct
 
  let deregister_image id ?region () =
    let params = [("ImageId", id)] in
    API.get "DeregisterImage" ~params (dereg_img_of_string : Ezxmlm.nodes -> 'a) ?region

  let register_image name ?image ?region () =
    let params = ("Name", name) in
    let params = match image with
      | Some image -> params::[("ImageLocation", image)]
      | None -> [params] in
    API.get "RegisterImage" ~params (reg_img_of_string : Ezxmlm.nodes -> 'a) ?region
 
end 

module EBS = struct

  let create_snapshot ?description id ?region () =
    let params = ("VolumeId", id) in
    let params = match description with
      | Some description -> params::[("Description", description)]
      | None -> [params] in
    API.get "CreateSnapshot" ~params create_snap_of_string ?region
	     
  let delete_volume id ?region () =
    let params = [("VolumeId", id)] in
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
    let params = [("InstanceId", id)] in
    API.get "GetConsoleOutput" ~params console_output_of_string ?region

  (* TODO this function has 1000 request parameters *)
  let run ?(min=1) ?(max=1) ?(instance="m1.small") ?zone ?kernel id ?region () =
    let params = match zone with
      | Some zone -> [("Placement.AvailabilityZone", zone)]
      | None -> [] in
    let params = match kernel with
      | Some kernel -> ("KernelId", kernel)::params
      | None -> params in
    let params = params@[("ImageId", id); ("MinCount", string_of_int min); ("MaxCount", string_of_int max)] in
    API.get "RunInstances" ~params run_instances_of_string ?region

  let start (ids : string list) ?region () =
    let params = Field.number_fields "InstanceId.%i" ids in
    API.get "StartInstances" ~params start_instances_of_string ?region

  let stop ?(force=false) (ids : string list) ?region () =
    let params = ("Force", string_of_bool force) in
    let params = params::(Field.number_fields "InstanceId.%i" ids) in
    API.get "StopInstances" ~params stop_instances_of_string ?region

  let terminate (ids : string list) ?region () = 
    let params = Field.number_fields "InstanceId.%i" ids in
    API.get "TerminateInstances" ~params terminate_instances_of_string ?region

end 

module Regions = struct
  
  let describe ?region () =
    API.get "DescribeRegions" ~params:[] ?region desc_regions_of_string
	    
end
