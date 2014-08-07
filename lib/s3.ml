open Signature4
       
let s3 = { service = "s3";
	   version = "2006-03-01"; }
	   
(* TODO *)
let handle_response (_,body) = 
  Lwt.bind (Cohttp_lwt_body.to_string body) Lwt_io.print
	   
let lwt_req {Monad.api; body; headers; meth; uri} =
  Cohttp_lwt_unix.Client.call ~headers ~body ~chunked:false meth uri
			      
let request req = 
  lwt resp = lwt_req req in
    handle_response resp       
		    
let put bucket obj =     
  let meth = `PUT in
  let uri = Uri.of_string (Printf.sprintf "https://%s.s3.amazonaws.com/" bucket) in
  let uri = Uri.with_path uri obj in 
  (* Yes this is totally weird but 
     see https://github.com/mirage/ocaml-uri/issues/51 *)
  let uri = Uri.of_string @@ Uri.to_string uri in
  let obj_contents_body = Cohttp_lwt_body.of_stream @@ Lwt_io.lines_of_file obj in
  lwt obj_contents_str = Cohttp_lwt_body.to_string @@ obj_contents_body in
    let headers = 
      (* TODO region *)
      realize_headers meth uri (obj_contents_str) s3 "us-west-2" ~acl:"authenticated-read" in 
    let headers = Cohttp.Header.add headers "x-amz-content-sha256" (Hash.hex_hash obj_contents_str) in
    request Monad.({ api = s3;
		     (* TODO just did a Cohttp_lwt_body.to_string... use obj-contents_body? *)
		     body = Cohttp_lwt_body.of_string obj_contents_str;
		     headers = headers;
		     meth = meth;
		     uri = uri; })
