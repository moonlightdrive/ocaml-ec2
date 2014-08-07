type api = {service: string; version: string; }

module Time = struct

  module C = CalendarLib.Calendar
  module P = CalendarLib.Printer.Calendar

  let date_yymmdd = P.sprint "%Y%m%d"

  let date_time = P.sprint "%Y%m%dT%H%M%SZ"

  let now_utc () = C.(now () |> to_gmt)

end

module Hash = struct

  open Cryptokit

  let hex_encode str = transform_string (Hexa.encode ()) str

  let sha256 ?k str = match k with
    | None -> hash_string (Hash.sha256 ()) str
    | Some k -> hash_string (MAC.hmac_sha256 k) str

  let hex_hash ?k str = hex_encode (sha256 ?k str)

end

module Signature = struct

  let iam_secret = Unix.getenv "AWS_SECRET_KEY"
  let iam_access = Unix.getenv "AWS_ACCESS_KEY"

  let signing_algorithm = "AWS4-HMAC-SHA256"

  let v4_req = "aws4_request"

  let content_type = "application/x-www-form-urlencoded; charset=utf-8" 

  let canonical_headers ?acl ~timestamp host= 
    let date = Time.date_time timestamp in
    let can_heads = 
      Printf.sprintf "content-type:%s\nhost:%s\n" content_type host in
    match acl with
    | None -> Printf.sprintf "%sx-amz-date:%s\n" can_heads date 
    | Some acl -> Printf.sprintf "%sx-amz-acl:%s\nx-amz-date:%s\n" can_heads acl date
				 
  let canonical_request meth ?acl ~timestamp ~host ~signed_headers ?(uri = "/") ?(query="") ?(payload="") () =
    let meth = Cohttp.Code.string_of_method meth in
    String.concat "\n" [meth; uri; query; canonical_headers ?acl ~timestamp host; signed_headers; Hash.hex_hash payload]
		  
  let credential_scope timestamp region service = String.concat "/" [Time.date_yymmdd timestamp; region; service; v4_req]
								
  let str_to_sign ~timestamp ~cred_scope ~req = 
    String.concat "\n" [signing_algorithm; Time.date_time timestamp; cred_scope; Hash.hex_hash req]

  let signature ~secret ~timestamp ~region str_to_sign service =
    let kSecret = "AWS4"^secret in
    let kDate = Hash.sha256 ~k:kSecret (Time.date_yymmdd timestamp) in
    let kRegion = Hash.sha256 ~k:kDate region in
    let kService = Hash.sha256 ~k:kRegion service in
    let kSigning = Hash.sha256 ~k:kService v4_req in
    Hash.hex_hash ~k:kSigning str_to_sign

end

let realize_headers ?acl meth uri body_str api region =
  let open Signature in 
  let acl_str = "x-amz-acl" in
  let signed_headers = ["content-type";"host";"x-amz-date"] |> 
			 fun sh -> (match acl with | None -> sh | Some acl -> acl_str::sh) |>
				     List.sort (String.compare) |> String.concat ";" in
  let timestamp = Time.now_utc () in
  let host = match Uri.host uri with
    | Some h -> h
    | None -> "ec2.amazonaws.com" in (* TODO don't hardcode this?? *)
  let secret = iam_secret in 
  let access = iam_access in
  let cred_scope = credential_scope timestamp region api.service in
  let credentials = access^"/"^cred_scope in
  let canonical_req = 
    let path = Uri.path @@ Uri.of_string @@ Uri.to_string uri in
    canonical_request meth ?acl ~uri:path ~timestamp ~host ~signed_headers ~payload:body_str () in
  let str_to_sign = str_to_sign ~timestamp ~cred_scope ~req:canonical_req in
  let signature = signature ~secret ~timestamp ~region str_to_sign api.service in
  let auth = 
    let to_string (f, v) = Printf.sprintf "%s=%s" f v in
    List.map to_string [ (signing_algorithm^" Credential", credentials)
		       ; ("SignedHeaders", signed_headers)
		       ; ("Signature", signature) 
		       ]
    |> String.concat ", " in
  let headers = Cohttp.Header.of_list [ "Authorization", auth;
					"Content-Type", content_type;
					"X-Amz-Date", Time.date_time timestamp; ] in
  match acl with 
  | None -> headers
  | Some acl -> Cohttp.Header.add headers acl_str acl
    
