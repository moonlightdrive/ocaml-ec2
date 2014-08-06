module API = struct

  open Signature4

  let s3 = { service = "s3";
	     version = "2006-03-01"; }

  let handle_response (_,body) = 
    Lwt.bind (Cohttp_lwt_body.to_string body) Lwt_io.print

  let lwt_req {Monad.api; body; headers; meth; uri} =
    Cohttp_lwt_unix.Client.call ~headers ~body ~chunked:false meth uri

  let request req = 
    lwt resp = lwt_req req in
    handle_response resp       
      
  let verb meth bucket obj =     
    let uri = Uri.of_string (Printf.sprintf "https://%s.s3.amazonaws.com/" bucket) in
    let headers = realize_headers meth uri obj s3 "us-west-2" in (* TODO region *)
    let headers = Cohttp.Header.add headers "x-amz-content-sha256" (Hash.hex_hash obj) in
    let headers = Cohttp.Header.add headers "x-am-acl" "public-read" in
    request Monad.({ api = s3;
		     body = Cohttp_lwt_body.of_string obj;
		     headers = headers;
		     meth = meth;
		     uri = uri; })
	    
  let get = verb `GET
  let put = verb `PUT

end
