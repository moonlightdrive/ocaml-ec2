module Util = struct
	     
  (* Turns ["Action","act";"Version","2014"] into "Action=act&Version=2014" *)
  let query_string params = 
    let string_of_field (f,v) = Printf.sprintf "%s=%s" f v in
    List.map string_of_field params |> String.concat "&"

  (* Turns "RegionNames.%i" ["eu-west-1";"us-east-1"] into
     [("RegionNames.1","eu-west-1"); ("RegionNames.2","us-east-1")] *)
  let number_fields pattern values =
    let rec number values n = match values with
      | [] -> []
      | (v::vs) -> Printf.sprintf pattern n :: (number vs (n+1)) in
    let names = number values 1 in
    List.combine names values
		 
  (* Same as number_fields above but pattern should be something like
     Printf.sprintf "Region.%i" *)
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
    let num_vals = 
      let f_vals = List.map snd fs in
      List.concat (number f_names f_vals 1 ) in
    let num_names = number_fields "Filter.%i.Name" f_names in
    List.rev_append num_vals num_names
		    
  let add_opt_param ?value name params = match value with 
    | Some v -> (name, v)::params 
    | None -> params
	     
end


(* TODO remove? *)
(* module Monad = struct

  type request = { api: Signature4.api; 
		   body: Cohttp_lwt_body.t;
		   headers: Cohttp.Header.t;
		   meth: Cohttp.Code.meth;
		   uri: Uri.t; }
		   
  type aws_error = { code: string;
		     msg: string; }
		     
  type error = 
    | Generic of Cohttp.Response.t * aws_error list
    | XML of string
    | No_response
	
  type 'a signal = 
    | Error of error
    | Response of 'a
   and 'a t = ('a signal) Lwt.t
			  
  let error e = Error e
n		      
  let response r = Response r
			    
  let awserrs_to_str errs = 
    let formatted = List.map (fun e -> Printf.sprintf "%s: %s" e.code e.msg) errs in
    String.concat "\n" formatted 
		  
  let error_to_string = function
    | Generic (http, aws) -> Printf.sprintf "HTTP Error %s\n%s" 
					    (Cohttp.Code.string_of_status (Cohttp.Response.status http)) (awserrs_to_str aws)
    | XML action -> Printf.sprintf "Some XML parsing error %s" action
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
		
end *)

module API = struct

  open Signature4

  let ec2 = { service = "ec2";
	      version = "2014-05-01"; }
	     
  (* for debugging *)
  let response_to_file action resp = 
    let dir = "/home/jsp/ocaml-ec2/lib_test/xml_responses" in
    let file = 
      let time = CalendarLib.(Time.now () |> Printer.Time.to_string) in
      let name = Printf.sprintf "%s-%s.xml" action time in
      Filename.concat dir name in
    print_endline @@ Printf.sprintf "created %s" file;
    Lwt_io.(with_file ~buffer_size:(String.length resp) ~mode:output file
		      (fun oc -> write oc resp))

  let handle_response action fn (envelope,body) = 
    let open Lwt in
    let open Ezxmlm in
    let response = "Response" in
    let actionresp = action^response in
    let parse_err (envelope, body) = 
      let awserr_of_str x : Monad.aws_error = 
	{ code = member "Code" x |> data_to_string;
	  msg = member "Message" x |> data_to_string ; } in	      
      let body = member response body in
      let errs = List.map awserr_of_str (member "Errors" body |> members "Error") in
      return @@ Monad.(error (Generic (envelope, errs))) in
    lwt (_,body) = Cohttp_lwt_body.to_string body >|= from_string in
      try 
	 return @@ Monad.response @@ fn @@ member actionresp body
      with 
      (* TODO warning this match case is unused *)
      | Tag_not_found actionresp -> parse_err (envelope, body)
      | Tag_not_found response -> return @@ Monad.(error @@ XML action)
											       
				     
  let lwt_req {Monad.api; body; headers; meth; uri} =
    Cohttp_lwt_unix.Client.call ~headers ~body ~chunked:false meth uri
				
  let request action fn req = 
    lwt resp = lwt_req req in
    handle_response action fn resp
		      
  let verb meth action fn ?region ~params = 
    let region = match region with
      | Some r ->  EC2_t.string_of_region r
      | None -> try Unix.getenv "REGION" with exn -> EC2_t.(string_of_region US_EAST_1) in
    let uri = Uri.of_string 
		(Printf.sprintf "https://ec2.%s.amazonaws.com/" region) in
    let body = Util.query_string (List.rev_append ["Action",action; "Version", ec2.version] 
						   params) in
    let headers = realize_headers meth uri body ec2 region in
    request action fn Monad.({ api = ec2;
			       body = Cohttp_lwt_body.of_string body;
			       headers = headers;
			       meth = meth;
			       uri = uri; })
	    
  let get action = verb `GET action
  let post action = verb `POST action 
		  
end

open EC2_t
open EC2_x

module AMI = struct

  let create_image ~name ?description id ?region () = 
    let params = [("InstanceID", InstanceID.to_string id); ("Name", name)] in
    let params = Util.add_opt_param ?value:description "Description" params in
    API.get "CreateImage" ~params create_img_of_string ?region
 
  let deregister_image id ?region () =
    let params = [("ImageId", ImageID.to_string id)] in
    API.get "DeregisterImage" ~params dereg_img_of_string ?region

  let register_image ~name ?img_path ?desc ?arch ?region () =
    let params = [("Name", name)] in
    let params = Util.add_opt_param ?value:img_path "ImageLocation" params 
		 |> Util.add_opt_param ?value:desc "Description"
		 |> Util.add_opt_param ?value:arch "Architecture" in
    API.get "RegisterImage" ~params reg_img_of_string ?region
 
end 

module EBS = struct

  let create_snapshot id ?description ?region () =
    let params = [("VolumeId", VolumeID.to_string id)] in
    let params = Util.add_opt_param ?value:description "Description" params in
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
      | Some ids -> params::(Util.number_fields "InstanceId.%i" ids) 
      | None -> [params] in
    API.get "DescribeInstanceStatus" ~params 
 *)
  let get_console_output id ?region () =
    let params = [("InstanceId", InstanceID.to_string id)] in
    API.get "GetConsoleOutput" ~params console_output_of_string ?region

  let run ?(min=1) ?(max=1) ?(instance="m1.small") ?zone ?kernel id ?region () =
    let params = Util.add_opt_param ?value:zone "Placement.AvailabilityZone" [] in
    let params = Util.add_opt_param ?value:kernel "KernelId" params in
    let params = params@[("ImageId", ImageID.to_string id);
			 ("MinCount", string_of_int min); 
			 ("MaxCount", string_of_int max);
			 ("InstanceType", instance);] in
    API.get "RunInstances" ~params run_instances_of_string ?region

  let start ids ?region () =
    let params = List.map InstanceID.to_string ids |>
		   Util.number_fields "InstanceId.%i" in
    API.get "StartInstances" ~params start_instances_of_string ?region

  let stop ?(force=false) ids ?region () =
    let params = ("Force", string_of_bool force) in
    let params = List.map InstanceID.to_string ids |> 
		   fun ids -> params::(Util.number_fields "InstanceId.%i" ids) in
    API.get "StopInstances" ~params stop_instances_of_string ?region

  let terminate ids ?region () = 
    let params = List.map InstanceID.to_string ids |>
		   Util.number_fields "InstanceId.%i" in
    API.get "TerminateInstances" ~params terminate_instances_of_string ?region

end 

module KeyPairs = struct

  let describe ?(names=[]) ?(filters=[]) ?region () = 
    let params = Util.number_fields "KeyName.%i" names in
    let params = List.rev_append params
				 (Util.format_filters filters) in
    API.get "DescribeKeyPairs" ~params desc_keys_of_string ?region

end

module Regions = struct

  let describe ?(regions=[]) ?(filters=[]) ?region () =
    let regions = Util.number_fields "RegionName.%i" (List.map string_of_region regions) in
    let filters = Util.format_filters filters in
    let params = List.rev_append regions filters in
    API.get "DescribeRegions" ~params desc_regions_of_string ?region
 	    
end
