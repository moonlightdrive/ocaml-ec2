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

type request = { api: Signature4.api; 
		 body: Cohttp_lwt_body.t;
		 headers: Cohttp.Header.t;
		 meth: Cohttp.Code.meth;
		 uri: Uri.t; }

type aws_error = { code: string;
		   msg: string; }
		 
type error = 
  | Generic of Cohttp.Response.t * aws_error list
  | XML of string * string
  | No_response
      
type 'a signal = 
  | Error of error
  | Response of 'a
 and 'a t = ('a signal) Lwt.t
			
let error e = Error e
		    
let response r = Response r
			
let awserrs_to_str errs = 
  let formatted = List.map (fun e -> Printf.sprintf "%s: %s" e.code e.msg) errs in
  String.concat "\n" formatted 

let error_to_string = function
  | Generic (http, aws) -> Printf.sprintf "HTTP Error %s\n%s" 
					  (Cohttp.Code.string_of_status 
					     (Cohttp.Response.status http)) 
					  (awserrs_to_str aws)
  | XML (action, tag) -> Printf.sprintf "Tag %s not found in %s response" tag action
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
