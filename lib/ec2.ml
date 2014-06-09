let service = "ec2"
		
let version = "2014-05-01"

let sample_access = "AKIAIOSFODNN7EXAMPLE"
let sample_secret = "wJalrXUtnFEMI/K7MDENG+bPxRfiCYEXAMPLEKEY"

let key name = Unix.getenv name
let iam_secret = key "AWS_SECRET_ACCESS_KEY"
let iam_access = key "AWS_ACCESS_KEY_ID"

module Time = struct

  open Core_extended.Std

  let date_yymmdd timestamp = Time.format timestamp "%Y%m%d"

  let date_time timestamp = Time.format timestamp "%Y%m%dT%H%M%SZ"

  (* TODO this is a horrible way to handle this *)
  let now_utc =
    let now = Time.now () in
    Time.sub now (Time.utc_offset now) 

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
  let query_string param_list = 
    let open Core.Std in (* is this necessary *)
    List.map ~f:to_string param_list |>
      List.intersperse ~sep:"&" |>
      String.concat ~sep:"" 

  (* Turns "RegionNames.%i" ["eu-west-1";"us-east-1"] into
     [("RegionNames.1","eu-west-1"); ("RegionNames.2","us-east-1")] *)
  let number_fields pattern values =
    let names = Core.Std.List.init (List.length values) ~f:(fun n -> Printf.sprintf pattern (n+1)) in
    Core.Std.List.zip_exn names values 
	     
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
    let auth_header = List.map Field.to_string [ (signing_algorithm^" Credential", credentials)
					       ; ("SignedHeaders", signed_headers)
					       ; ("Signature", signature) 
					       ]
		      |> String.concat ", " in
    Cohttp.Header.of_list [ "Authorization", auth_header
			  ; "Content-Type", content_type
			  ; "X-Amz-Date", Time.date_time timestamp 
			  ]

let make_req meth headers body uri = Cohttp_lwt_unix.Client.call ~headers ~body ~chunked:false meth uri

let handle_response (envelope,body) =
(*    Lwt.return (Cohttp_lwt_body.to_string body)*)
  Lwt.bind (Cohttp_lwt_body.to_string body) Lwt_io.print
(*  let status = Cohttp_lwt_unix.Response.status envelope in
  Lwt.return (Cohttp.Code.string_of_status status |> print_endline)
 *)

let verb meth action ?(region="us-east-1") ~params =
  let params = param_list action ~params in
  let headers = realize_headers meth action ~params ~region in
  let body = realize_body params in
  let uri = URI.base region in
  Lwt.bind (make_req meth headers body uri) handle_response

let get = verb `GET 

let post = verb `POST
    
end

module AMI = struct
  
  let deregister_image id =
    let params = [("ImageId", id)] in
    API.get "DeregisterImage" ~params

  let register_image name ?image =
    let params = ("Name", name) in
    let params = match image with
      | Some image -> params::[("ImageLocation", image)]
      | None -> [params] in
    API.get "RegisterImage" ~params

end

module EBS = struct
	     
  let delete_volume id =
    let params = [("VolumeId", id)] in
    API.post "DeleteVolume" ~params

end

module Instances = struct

  (* TODO implement the remaining parameters *)
  let describe_status ?ids ?(all=false) () =
    let params = ("IncludeAllInstances", string_of_bool all) in
    let params = match ids with 
      | Some ids -> params::(Field.number_fields "InstanceId.%i" ids) 
      | None -> [params] in
    API.get "DescribeInstanceStatus" ~params

  let start ids =
    let params = Field.number_fields "InstanceId.%i" ids in
    API.post "StartInstances" ~params	     

  let stop ?(force=false) ids =
    let params = ("Force", string_of_bool force) in
    let params = params::(Field.number_fields "InstanceId.%i" ids) in
    API.post "StopInstances" ~params

  let get_console_output id =
    let params = [("InstanceId", id)] in
    API.post "GetConsoleOutput" ~params

end
	       
module Regions = struct
  
  let describe = 
    API.get "DescribeRegions" ~params:[]
	    
end
