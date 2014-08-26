(* 
 * Copyright (c) 2014, Jyotsna Prakash <jyotsna.prakash@gmail.com>
 *
 * Permission to use, copy, modify, and/or distribute this software for any 
 * purpose with or without fee is hereby granted, provinded that the above 
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES 
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 *)

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

module API = struct

  open Signature4

  let ec2 = { service = "ec2";
	      version = "2014-05-01"; }
	     
  (* for debugging *)
  let response_to_file action resp = 
    let dir = "lib_test/xml_responses" in
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
    (* response_to_file action (envelope, body); *)
    let response = "Response" in
    let actionresp = action^response in
    let parse_err (envelope, body) = 
      let awserr_of_str x : Monad.aws_error = 
	{ code = member "Code" x |> data_to_string;
	  msg = member "Message" x |> data_to_string ; } in	      
      try let body = member response body in
	  let errs = List.map awserr_of_str (member "Errors" body |> members "Error") in
	  return @@ Monad.(error @@ Generic (envelope, errs))
      with exn -> return @@ Monad.(error @@ XML action) in
    lwt (_,body) = Cohttp_lwt_body.to_string body >|= from_string in
    try return @@ Monad.response @@ fn @@ member actionresp body 
    with exn -> parse_err (envelope, body)
				     
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
			       headers; meth; uri; })
	    
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

  let create name ?region () = failwith "undefined"

  let delete name ?region () = 
    let params = [("KeyName", name)] in
    API.post "DeleteKeyPair" ~params del_key_of_string ?region

  let describe ?(names=[]) ?(filters=[]) ?region () = 
    let params = Util.number_fields "KeyName.%i" names in
    let params = List.rev_append params
				 (Util.format_filters filters) in
    API.get "DescribeKeyPairs" ~params desc_keys_of_string ?region

  let import ? region () = failwith "undefined"

end

module Regions = struct

  let describe ?(regions=[]) ?(filters=[]) ?region () =
    let regions = Util.number_fields "RegionName.%i" (List.map string_of_region regions) in
    let filters = Util.format_filters filters in
    let params = List.rev_append regions filters in
    API.get "DescribeRegions" ~params desc_regions_of_string ?region

  let describe_zones ?(zones=[]) ?(filters=[]) ?region () =
    let zones = Util.number_fields "ZoneName.%i" zones in
    let filters = Util.format_filters filters in
    let params = List.rev_append zones filters in
    API.get "DescribeAvailabilityZones" ~params desc_zones_of_string ?region
 	    
end
