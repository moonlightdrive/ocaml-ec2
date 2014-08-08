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
		 
let put ?region bucket obj =     
  let meth = `PUT in
  let uri = Uri.of_string (Printf.sprintf "https://%s.s3.amazonaws.com/" bucket) |>
	      fun uri -> Uri.with_path uri obj |>
			   (* Yes this is totally weird but 
                              see https://github.com/mirage/ocaml-uri/issues/51 *)
			   Uri.to_string |> Uri.of_string in
  lwt body = 
    let open Lwt_io in
    let open Lwt in
    lwt len = Lwt_io.file_length obj >|= Int64.to_int in
    let buffer = String.create len in
    Lwt_io.with_file ~buffer_size:len ~mode:input obj 
		     (fun ic -> Lwt_io.read_into_exactly ic buffer 0 len)
    >>= fun () -> return buffer in
  let headers = 
    (* TODO region *)
    let region = match region with
      | Some r -> EC2_t.string_of_region r
      | None -> match Sys.getenv "REGION" with exn -> EC2_t.(string_of_region US_EAST_1) in
    realize_headers meth uri body s3 region ~acl:"authenticated-read" in 
  let headers = Cohttp.Header.add headers "x-amz-content-sha256" (Hash.hex_hash body) in
  request Monad.({ api = s3;
		   body = Cohttp_lwt_body.of_string body;
		   headers = headers;
		   meth = meth;
		   uri = uri; })
