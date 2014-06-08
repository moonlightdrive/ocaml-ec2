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

module Monad = struct
(*
type request = {
  meth: Cohttp.Code.meth; uri: Uri.t;
  headers: Cohttp.Header.t; body: Cohttp_lwt_body.t;
}
 *)

end

module API = struct

  let service = "ec2"

  let version = "2014-05-01"

  let signing_algorithm = "AWS4-HMAC-SHA256"

  let v4_req = "aws4_request"

  let content_type = "application/x-www-form-urlencoded; charset=utf-8"

  (* Turns ("Action", "act") into "Action=act" *)
  let string_of_param (k,v) = Printf.sprintf "%s=%s" k v

  (* Turns ["Action","act";"Version","2014"] into "Action=act&Version=2014" *)
  let string_of_params param_list = 
    let open Core.Std in (* is this necessary *)
    List.map ~f:string_of_param param_list |>
      List.intersperse ~sep:"&" |>
      String.concat ~sep:"" 

  let signed_headers = "content-type;host;x-amz-date"

  let canonical_headers ~timestamp host = 
    let date = Time.date_time timestamp in
    Printf.sprintf "content-type:%s\nhost:%s\nx-amz-date:%s\n" content_type host date

  let canonical_request meth ~timestamp ~host ?(uri = "/") ?(query="") ?(payload="") () =
    let meth = Cohttp.Code.string_of_method meth in
    String.concat "\n" [meth; uri; query; canonical_headers timestamp host; signed_headers; Hash.hex_hash payload]

  let credential_scope t r () = String.concat "/" [Time.date_yymmdd t; r; service; v4_req]

  let str_to_sign ~timestamp ~cred_scope ~req = 
    String.concat "\n" [signing_algorithm; Time.date_time timestamp; cred_scope; Hash.hex_hash req]

  let signature ~secret ~timestamp ~region str_to_sign =
    let kSecret = "AWS4"^secret in
    let kDate = Hash.sha256 ~k:kSecret (Time.date_yymmdd timestamp) in
    let kRegion = Hash.sha256 ~k:kDate region in
    let kService = Hash.sha256 ~k:kRegion service in
    let kSigning = Hash.sha256 ~k:kService v4_req in
    Hash.hex_hash ~k:kSigning str_to_sign
		     
  let sample_access = "AKIAIOSFODNN7EXAMPLE"
  let sample_secret = "wJalrXUtnFEMI/K7MDENG+bPxRfiCYEXAMPLEKEY"

	     
  let param_list action (* ?(additional=[]) *) = 
    (*rev_append additional*) ["Action", action; "Version", version]
			    
  let realize_headers meth action ~params ~region =
    let timestamp = Time.now_utc in
    let host = URI.host region in
    let secret = sample_secret in
    let access = sample_access in
    let cred_scope = credential_scope timestamp region () in
    let credentials = access^"/"^cred_scope in
    let body = string_of_params params in
    let canonical_req = canonical_request meth ~timestamp ~host ~payload:body () in
    let str_to_sign = str_to_sign ~timestamp ~cred_scope ~req:canonical_req in
    let signature = signature ~secret ~timestamp ~region str_to_sign in
    let auth_header = List.map string_of_param [(signing_algorithm^" Credential", credentials); ("SignedHeaders", signed_headers); ("Signature", signature)]
		      |> String.concat ", " in
    Cohttp.Header.of_list [ "Authorization", auth_header
				  ; "Content-Type", content_type
				  ; "X-Amz-Date", Time.date_time timestamp
				  ]
			  
  let get action = 
    let meth = `GET in
    let params = param_list action in
    let region = "us-west-2" in
    let headers = realize_headers meth action ~params ~region in
    let body = Cohttp_lwt_body.of_string (string_of_params params) in
    let uri = URI.base region in
    Cohttp_lwt_unix.Client.call ~headers ~body ~chunked:false meth uri
      
end
	       
module Regions = struct
  
  let describe = 
    API.get "DescribeRegions"
	    
end


let _ = 
(*  let region = "us-west-2" in*)
  Lwt_main.run (Regions.describe)
