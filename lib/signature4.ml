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

type api = {service: string; version: string; }

module Time = struct

  module C = CalendarLib.Calendar
  module P = CalendarLib.Printer.Calendar

  let date_yymmdd = P.sprint "%Y%m%d"

  let date_time = P.sprint "%Y%m%dT%H%M%SZ"

  let now_utc () = C.(now () |> to_gmt)

end

module Hash = struct

  open Cstruct
  open Nocrypto.Hash.SHA256

  let hex_encode str = 
    let remove ch str = String.concat "" @@ Stringext.split ~on:ch str in
    let buf = Buffer.create 32 in
    of_string str |>
      hexdump_to_buffer buf;
    remove ' ' @@ remove '\n' @@ Buffer.contents buf

  let sha256 ?k str = match k with
    | None -> to_string @@ digest (of_string str)
    | Some k -> to_string @@ hmac ~key:(of_string k) (of_string str) 

  let hex_hash ?k str = hex_encode (sha256 ?k str)

end

(* Something below this is giving us an Exception: Not_found issue.... *)

module Signature = struct
  let retrieve_key s = try Unix.getenv s
    with Not_found -> failwith @@ Printf.sprintf "Environment variable %s not found." s

  let iam_secret = retrieve_key "AWS_SECRET_KEY"
  let iam_access = retrieve_key "AWS_ACCESS_KEY"

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

