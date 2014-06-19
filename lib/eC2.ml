let service = "ec2"
		
let version = "2014-05-01"


let iam_secret = Unix.getenv "AWS_SECRET_KEY"
let iam_access = Unix.getenv "AWS_ACCESS_KEY"

module Time = struct

  module C = CalendarLib.Calendar
  module P = CalendarLib.Printer.Calendar

  let date_yymmdd = P.sprint "%Y%m%d"

  let date_time = P.sprint "%Y%m%dT%H%M%SZ"

  let now_utc = C.(now () |> to_gmt)

end

module Hash = struct

  open Cryptokit

  let hex_encode str = transform_string (Hexa.encode ()) str

  let sha256 ?k str = match k with
    | None -> hash_string (Hash.sha256 ()) str
    | Some k -> hash_string (MAC.hmac_sha256 k) str

  let hex_hash ?k str = hex_encode (sha256 ?k str)

end

module URI = struct

  let host region = Printf.sprintf "ec2.%s.amazonaws.com" region

  let base region = Uri.of_string (Printf.sprintf "https://%s/" (host region))
				      
end

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

module Signature = struct

  let signing_algorithm = "AWS4-HMAC-SHA256"

  let v4_req = "aws4_request"

  let content_type = "application/x-www-form-urlencoded; charset=utf-8"

  let signed_headers = "content-type;host;x-amz-date"

  let canonical_headers ~timestamp host = 
    let date = Time.date_time timestamp in
    Printf.sprintf "content-type:%s\nhost:%s\nx-amz-date:%s\n" content_type host date
		 
  let canonical_request meth ~timestamp ~host ?(uri = "/") ?(query="") ?(payload="") () =
    let meth = Cohttp.Code.string_of_method meth in
    String.concat "\n" [meth; uri; query; canonical_headers timestamp host; signed_headers; Hash.hex_hash payload]

  let credential_scope timestamp region = String.concat "/" [Time.date_yymmdd timestamp; region; service; v4_req]

  let str_to_sign ~timestamp ~cred_scope ~req = 
    String.concat "\n" [signing_algorithm; Time.date_time timestamp; cred_scope; Hash.hex_hash req]

  let signature ~secret ~timestamp ~region str_to_sign =
    let kSecret = "AWS4"^secret in
    let kDate = Hash.sha256 ~k:kSecret (Time.date_yymmdd timestamp) in
    let kRegion = Hash.sha256 ~k:kDate region in
    let kService = Hash.sha256 ~k:kRegion service in
    let kSigning = Hash.sha256 ~k:kService v4_req in
    Hash.hex_hash ~k:kSigning str_to_sign

end

module Monad = struct

  type request = { meth: Cohttp.Code.meth;
		   headers: Cohttp.Header.t;
		   body: Cohttp_lwt_body.t; 
		   uri: Uri.t;
		   aws_action: string;
		 }

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
         
  let param_list ~params action =
    List.rev_append ["Action", action; "Version", version] params

  let realize_body field_pairs = Cohttp_lwt_body.of_string (Field.query_string field_pairs)
				     
  let realize_headers meth action ~params ~region =
    let open Signature in
    let timestamp = Time.now_utc in
    let host = URI.host region in
    let secret = iam_secret in
    let access = iam_access in
    let cred_scope = credential_scope timestamp region in
    let credentials = access^"/"^cred_scope in
    let body = Field.query_string params in
    let canonical_req = canonical_request meth ~timestamp ~host ~payload:body () in
    let str_to_sign = str_to_sign ~timestamp ~cred_scope ~req:canonical_req in
    let signature = signature ~secret ~timestamp ~region str_to_sign in
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

  let lwt_req {Monad.meth; headers; body; uri; aws_action} = 
    Cohttp_lwt_unix.Client.call ~headers ~body ~chunked:false meth uri

  let request fn req = 
    lwt resp = lwt_req req in
    handle_response req.aws_action fn resp

  (* TODO default region not working *)
  let verb meth action fn ?(region="us-east-1") ~params = 
    let params = param_list action ~params in
    let headers = realize_headers meth action ~params ~region in
    let body = realize_body params in
    let uri = URI.base region in
    request fn Monad.({ meth = meth;
		 headers = headers;
		 body = body;
		 uri = uri;
		 aws_action = action; })
	    
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
