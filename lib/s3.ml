let service = "s3"

let version = "2006-03-01"

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

let uri bucket = Printf.sprintf "%s.s3.amazonaws.com/" bucket

(*let obj = "mir-img.manifest.xml" *)

let verb meth obj= 
  let open Signature in
  let timestamp = Time.now_utc in
  let secret = iam_secret in
  let access = iam_access in
  let region = "us-west-2" in
  let cred_scope = credential_scope timestamp region in
  let credentials = access^"/"^cred_scope in
  let body = obj in
  let host = uri "jymirage" in
  let canonical_req = canonical_request meth ~timestamp ~host ~payload:body () in
  let str_to_sign = str_to_sign ~timestamp ~cred_scope ~req:canonical_req in
  let signature = signature ~secret ~timestamp ~region str_to_sign in
  let auth = List.map Field.to_string [ (signing_algorithm^" Credential", credentials)
				      ; ("SignedHeaders", signed_headers)
				      ; ("Signature", signature) 
				      ]
	     |> String.concat ", " in  
  let headers = Cohttp.Header.of_list [ "x-amz-date", Time.date_time timestamp; 
				(*	"Host", host;*)
					"Authorization", auth;  
					"Content-Type", content_type;
					"x-amz-content-sha256", Hash.hex_hash obj;
				      ] in
  let body = Cohttp_lwt_body.of_string (body) in
(*  let uri = Uri.with_path (Uri.of_string ("https://"^host)) ("/"^obj) in*)
  let uri = Uri.of_string "https://jymirage.s3.amazonaws.com/" in
  let req = Cohttp_lwt_unix.Client.call ~headers ~body ~chunked:false meth uri in
(*  req*)
  let curl = "curl -v -X "^(Cohttp.Code.string_of_method meth)
	     ^ " -d "^obj
	     ^ " -H x-amz-date=\""^(Time.date_time timestamp)^"\""
	     ^ " -H Authorization=\""^auth^"\""
	     ^ " -H Content-type=\""^content_type^"\""
	     ^ " -H x-amz-content-sha256=\""^(Hash.hex_hash obj)^"\""
	     ^ " https://jymirage.s3.amazonaws.com/" in
  print_endline curl 


let get obj = verb `GET obj
let put obj = verb `PUT obj


(*
let _ = 
  let (_,resp_body) = Lwt_main.run (get "mirage.image.manifest.xml") in
  Lwt.bind (Cohttp_lwt_body.to_string resp_body) Lwt_io.print
 *)

let _ = get "mirage.image.manifest.xml"

