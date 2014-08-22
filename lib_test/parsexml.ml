open EC2
open Lwt

let (fake : Cohttp.Response.t) =
  { encoding = Cohttp.Transfer.Chunked;
    headers = Cohttp.Header.of_list [];
    version = `HTTP_1_1;
    status = `Accepted;
    flush = false; }
 
let test (action, (fn : Ezxmlm.nodes -> 'a)) = 
  let file = Printf.sprintf "xml_responses/%s.xml" action in
  lwt filestr = 
    let open Lwt_io in
    with_file ~mode:input file read >|= Cohttp_lwt_body.of_string in
    print_endline @@ Printf.sprintf "\n%s test" action;
    API.handle_response action fn (fake, filestr)

let run_test t =
  Lwt_main.run @@ Monad.run @@ test t;
  print_endline @@ "..done"

open EC2_x

let _ = run_test ("DescribeRegions", desc_regions_of_string)
let _ = run_test ("RunInstances", run_instances_of_string)
