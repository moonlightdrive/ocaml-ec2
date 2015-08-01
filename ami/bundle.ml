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

(* UTILS *)
let hex str =
  let open Cstruct in
  let remove ch str = String.concat "" @@ Stringext.split ~on:ch str in
  let buf = Buffer.create 32 in
  of_string str |>
    hexdump_to_buffer buf;
  remove ' ' @@ remove '\n' @@ Buffer.contents buf

let tmp = Filename.(concat @@ get_temp_dir_name ())

let cs_of_file f = Unix_cstruct.of_fd Unix.(openfile f [O_RDONLY] 0)

let name_only f = Filename.(chop_extension @@ basename f)

let filesize f = let open Unix in (stat f).st_size

let format_ssl_output s = 
  let open String in
  let ofs = index s ' ' in
  trim @@ sub s ofs (length s - ofs)


(* XML tree things *)
type tree = E of Xmlm.tag * tree list | D of string

let add_attr el attr = match el with
  | E ((t, attrs), cs) -> E ((t, attr::attrs), cs)
  | D _ -> invalid_arg "cannot add attribute to data"

let el n ?(attrs=[]) cs = 
  let to_attribute (a,v) = ("",a),v in
  let attrs = List.map to_attribute attrs in
  let tag n = ("", n), attrs in
  E (tag n, cs)

let data d = D d

let tree_concat t branch = 
  match t with 
  | D _ -> invalid_arg "can't concat two branches"
  | E (t, bs) -> E (t, branch::bs)

let tree_concatmap parent f ts =
  List.map f ts |>
    fun ts -> List.fold_right tree_concat ts parent


(* XML attribute constants *)
let aes128cbc = ["algorithm","AES-128-CBC"]
let digest = ["algorithm","SHA1"]

(* Manifest types and tree creation *)
type bundler = { name: string;
		 version: string;
		 release: string; }

let tree_of_bundler b = 
  el "bundler" [ el "name" [data b.name];
		 el "version" [data b.version];
		 el "release" [data b.release]; ]

type machine_config = { arch: string; kernel: string; }

let tree_of_machconf m =
  el "machine_configuration" [ el "architecture" [data m.arch]; 
			       el "kernel_id" [data m.kernel]; ]

type part = { filename: string; digest: string; }

let tree_of_parts ps =
  let open List in
  let len = length ps in
  let rec range a b = match a == b with | true -> [] | false -> a::(range (succ a) b) in
  let ps = combine ps (map string_of_int @@ range 0 len) in
  let of_part (p, i) =
    el "part" ~attrs:["index",i] [ el "filename" [data p.filename];
				   el "digest" ~attrs:digest [data p.digest]; ] in
  List.map of_part ps |>
    (* do you really want a foldl here *)
    List.fold_left tree_concat (el "parts" ~attrs:["count", string_of_int len] [])

type img = 
    { name: string;
      user: string;
      img_type: string;
      digest: string;
      size: int; 
      b_size: int;
      ec2_enc_key : string;
      user_enc_key : string;
      ec2_enc_iv: string;
      user_enc_iv: string;
      parts: part list;  
    }
		      
let tree_of_img i =
  el "image" [ el "name" [data i.name];
	       el "user" [data i.user];
	       el "type" [data i.img_type];
	       el "digest" ~attrs:digest [data i.digest];
	       el "size" [data @@ string_of_int i.size];
               el "bundled_size" [data @@ string_of_int i.b_size];
	       el "ec2_encrypted_key" ~attrs:aes128cbc [data i.ec2_enc_key];
	       el "user_encrypted_key" ~attrs:aes128cbc [data i.user_enc_key];
	       el "ec2_encrypted_iv" [data i.ec2_enc_iv];
	       el "user_encrypted_iv" [data i.user_enc_iv];
	       tree_of_parts i.parts;
	     ]

type manifest = { version: string;
		  bundler: bundler;
		  machine_config: machine_config;
		  image: img;
		  signature: string;
		}

(* constants *)
let manivers = "2007-10-10"		  
let bundler = { name = "ocaml-ec2"; version = "0.1"; release = "0.1"; }
let machconf kernel = { arch = "x86_64"; kernel; }

let digest_parts = 
  let digest p = cs_of_file p |> 
		   fun s -> Nocrypto.Hash.(digest `SHA1 s) |> 
		   Cstruct.to_string |> hex in
  let to_part p = { filename = Filename.basename p; digest = digest p;} in
  List.map to_part 

let pub_enc key msg =
  Nocrypto.Rsa.PKCS1.encrypt ~key (Cstruct.of_string msg) |> Cstruct.to_string |> hex

let pubkey_of_cert file = 
  Unix_cstruct.of_fd Unix.(openfile file [O_RDONLY] 0) |>
  X509.Encoding.Pem.Certificate.of_pem_cstruct1 |>
  X509.public_key |> function
  | `RSA pub -> pub
  | `EC_pub _ -> invalid_arg "No public RSA key in certificate"

let split file = 
  let open Lwt in
  let open Lwt_io in
  let chunksize = 1024 * 1024 * 10 in
  lwt filelen = file_length file >|= Int64.to_int in
  let ic = open_in_bin file in
  let buffer = Bytes.create filelen in
  really_input ic buffer 0 filelen;
  close_in ic;
  let basename = Filename.(chop_suffix file ".tar.gz.enc" |> basename) in
  let rec aux n ps buf () =
    let rem = String.length buf in
    let len = match rem < chunksize with true -> rem | false -> chunksize in
    let part = tmp @@ Printf.sprintf "%s.part.%i" basename n in
    let oc = open_out_bin part in
    Pervasives.output oc buf 0 len;
    close_out oc;
    let newbuf = Bytes.create (rem - len) in
    match newbuf with
    | "" -> return @@ List.rev (part::ps)
    | s -> String.blit buf len newbuf 0 (String.length newbuf);
	   aux (succ n) (part::ps) newbuf () in
  aux 0 [] buffer ()

let img_of_file ~aeskey ~iv ~cert ~digest ~parts ?user ?ec2_cert file = 
  let open Filename in
  let name = basename file in
  let bundle = tmp @@ Printf.sprintf "%s.tar.gz.enc" name in
  let user = match user with
    | Some u -> u
    | None -> try Sys.getenv "AWS_USER" 
	      with exn -> failwith "env variable AWS_USER not set" in
  let size = filesize file in
  let b_size = filesize bundle in
  let ec2_pub_key = 
    let ec2_cert = match ec2_cert with 
      | Some c -> c
      | None -> let cert = "ec2/cert-ec2.pem" in
		let dir = BaseStandardVar.datadir () in
		Filename.concat dir cert in
    pubkey_of_cert ec2_cert in
  let user_pub_key = pubkey_of_cert cert in 
  let ec2_enc_key = pub_enc ec2_pub_key aeskey in
  let user_enc_key = pub_enc user_pub_key aeskey in
  let ec2_enc_iv = pub_enc ec2_pub_key iv in
  let user_enc_iv = pub_enc user_pub_key iv in
  { name; user;
    img_type = "machine"; digest;
    size; b_size; 
    ec2_enc_key; user_enc_key;
    ec2_enc_iv; user_enc_iv; 
    parts; }
        
(*
Pipeline.execute: command = [/bin/bash -c 'openssl sha1 < /tmp/ec2-bundle-image-digest-pipe-10174 & tar -c -h -S --owner 0 --group 0 -C /home/jsp mymirage.img | tee /tmp/ec2-bundle-image-digest-pipe-10174 | gzip -9 | openssl enc -e -aes-128-cbc -K a3890552c945121fd49f0f08bfcc0a55 -iv 7e31f208a4211a2f46126b6b07d5e461 > ec2_tmp/mymirage.img.tar.gz.enc; echo ${PIPESTATUS[0]} > /tmp/image-bundle-pipeline-pipestatus-020140728-10174-1l636zs & echo ${PIPESTATUS[1]} > /tmp/image-bundle-pipeline-pipestatus-120140728-10174-1y9khin & echo ${PIPESTATUS[2]} > /tmp/image-bundle-pipeline-pipestatus-220140728-10174-1ys78ae & echo ${PIPESTATUS[3]} > /tmp/image-bundle-pipeline-pipestatus-320140728-10174-1p9bng1']

Pipeline.execute: output = [(stdin)= 37dc731c9f9c71de2310cb249c607726f3d8eabd]
 *)
let digest_pipe = tmp @@ Printf.sprintf "image-bundle-pipeline-%i" (Unix.getpid ())

let bundle f ~aeskey ~iv () = 
  let open Unix in
  let dir = Filename.dirname f in
  let encrypted_dest = tmp @@ Printf.sprintf "%s.tar.gz.enc" (Filename.basename f) in
  (* TODO if things fail this digest pipe isn't being deleted but it needs to be! *)
  mkfifo digest_pipe 0o666;
  let pipeline () = 
    let tar_expand = 
      let options = Printf.sprintf "-c -h -S --owner 0 --group 0 -C %s" dir
      and files = Filename.basename f in
      Printf.sprintf "tar %s %s" options files in
    let cmd = Printf.sprintf 
		"openssl sha1 < %s & %s | tee %s | gzip -9 | openssl enc -e -aes-128-cbc -K %s -iv %s > %s" 
		digest_pipe tar_expand digest_pipe aeskey iv encrypted_dest in
    let ic = open_process_in cmd in
    let digest = input_line ic in
    ignore @@ Unix.close_process_in ic;
    format_ssl_output digest in
  pipeline ()
	   
(* sha1 with private key*)
let sign keyfile kernel i =
  let b = Buffer.create 1024 in (* closer to 2198 *)
  let oc = Xmlm.(make_output (`Buffer b) ~decl:false) in
  let out = Xmlm.output oc in
  let o_tree = 
    let frag = function
      | E (tag, childs) -> `El (tag, childs) 
      | D d -> `Data d in
    Xmlm.output_tree frag oc in
  let key = X509.Encoding.Pem.Private_key.of_pem_cstruct1 @@ cs_of_file keyfile |>
  function `RSA priv -> priv in
  out (`Dtd None);
  o_tree @@ tree_of_machconf @@ machconf kernel;
  out (`Dtd None);
  o_tree @@ tree_of_img i; 
  Buffer.contents b |> Cstruct.of_string |>
    Nocrypto.Hash.SHA1.digest |>
    Nocrypto.Rsa.PKCS1.sig_encode ~key

let manifest_of_file f ?user ?ec2_cert ~key ~cert ~aeskey ~iv ~kernel ~digest ~parts = 
  let image = img_of_file ~aeskey ~iv ~cert ~digest ~parts ?user ?ec2_cert f in
  let signature = Cstruct.to_string (sign key kernel image) |> hex in
  { version = manivers;
    bundler;
    machine_config = machconf kernel;
    image;
    signature; }

let tree_of_manifest m = 
  el "manifest" [ el "version" [data m.version];
		  tree_of_bundler m.bundler;
		  tree_of_machconf m.machine_config;
		  tree_of_img m.image;
		  el "signature" [data m.signature]; ]

(* name = "smth.img", m : manifest *) 
let create_manifest name m = 
  let open Xmlm in
  (* TODO allow user to specify name *)
  let manidest = tmp @@ Printf.sprintf "%s.manifest.xml" name in
  let oc = open_out manidest in
  let o = make_output (`Channel oc) in
  let out = output o in
  let frag = 
    function
    | E (tag, childs) -> `El (tag, childs) 
    | D d -> `Data d in
  let o_tree = output_tree frag o in
  out (`Dtd None);
  o_tree m;
  close_out oc;
  manidest

let clean_up f () = 
  let to_delete = [digest_pipe; 
		   tmp @@ Printf.sprintf "%s.tar.gz.enc" @@ Filename.basename f] in
  ignore @@ List.map Unix.unlink to_delete 

(* This is largely based on ocaml-tls [1]
   I need to find out if this is the proper way to do this 
https://github.com/mirleft/ocaml-tls/blob/ee1abcb2ab32e1677a1f869c4692db00af8c9403/lwt/tls_lwt.ml *)

let seed ?(device="/dev/urandom") () =
  let open Lwt in
  lwt fd = Lwt_unix.(openfile device [O_RDONLY] 0) in
  let buf = Cstruct.create 32 in
  Lwt_cstruct.(complete (read fd) buf) >|= fun () ->
    Nocrypto.Rng.reseed buf

let bundle_img ~key ~cert ~kernel ?ec2_cert ?user f = 
  let open Lwt in
  let basename = Filename.basename in
  ignore_result @@ seed ();
  let gen_key () = Nocrypto.Rng.generate 16 |> Cstruct.to_string |> hex in
  let aeskey = gen_key () in
  let iv = gen_key ()  in  
  let digest = bundle ~aeskey ~iv f () in
  let bundle = tmp @@ Printf.sprintf "%s.tar.gz.enc" @@ basename f in
  lwt parts = bundle |> split >|= digest_parts in
  let manifestdest = manifest_of_file f ?user ?ec2_cert ~key ~cert ~aeskey ~iv ~kernel ~digest ~parts |> 
		       tree_of_manifest |>
		       create_manifest (basename f) in
  clean_up f ();
  let parts_paths = List.map (fun p -> tmp @@ p.filename) parts in
  return (manifestdest, parts_paths)

let upload ?region (m, ps) ~bucket = 
  List.map (fun f -> S3.put ?region bucket f) (m::ps)
