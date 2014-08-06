(* THIS OPERATOR
Lwt.(>|=);;
- : 'a Lwt.t -> ('a -> 'b) -> 'b Lwt.t = <fun>
 *)

(* Bundle ami *)
#require "cryptokit";;

#require "x509,nocrypto,xmlm";;

#require "lwt,lwt.syntax,cstruct.unix";;


(* UTILS *)
let hex = Cryptokit.(transform_string @@ Hexa.encode ())

let tmp = Filename.(concat @@ get_temp_dir_name ())

let cs_of_file f = Unix_cstruct.of_fd Unix.(openfile f [O_RDONLY] 0)

let name_only f = Filename.(chop_extension @@ basename f)

let filesize f = let open Unix in (stat f).st_size

let write_file dest str = 
  let oc = open_out dest in
  output_string oc str;
  close_out oc

let format_ssl_output s = 
  let open String in
  let ofs = index s ' ' in
  trim @@ sub s ofs (length s - ofs)


(*
(* TODO why is this only writing 65536 bytes? *)
let split file =
  let open Unix in
  let chunk_size = 1024 * 1024 * 10 in
  let buffer = String.create chunk_size in
  let fd_in = openfile file [O_RDONLY] 0 in 
  let rec copy_loop n ps () = 
    let part = 
      let name = Filename.(chop_extension @@ chop_extension @@ name_only file) in
      tmp @@ Printf.sprintf "%s.img.part.%i" name n in
    let ch_out = openfile part [O_WRONLY; O_CREAT; O_TRUNC] 0o666 in
    match read fd_in buffer (n * chunk_size) buffer_size with 
    | 0 -> close fd_in; ps
    | r -> print_endline @@ Printf.sprintf "writing %i bytes" r;
	   ignore (write ch_out buffer (n * chunk_size) r); 
	   close ch_out; 
	   copy_loop (succ n) (part::ps) () in 
  copy_loop 0 [] ()
  *)

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
let mymachconf = { arch = "x86_64"; kernel = "aki-fc8f11cc"; }


(* This function is entirely correct *)
let digest_parts = 
  let digest p = cs_of_file p |> 
		   fun s -> Nocrypto.Hash.(digest `SHA1 s) |> 
		   Cstruct.to_string |> hex in
  let to_part p = { filename = Filename.basename p; digest = digest p;} in
  List.map to_part 

(* RSA PKCS1 *)
let pub_enc key msg =
  Nocrypto.RSA.PKCS1.encrypt ~key (Cstruct.of_string msg) |> Cstruct.to_string |> hex
	     
let pubkey_of_cert file = 
  let open Asn_grammars in
  Unix_cstruct.of_fd Unix.(openfile file [O_RDONLY] 0) |>
    X509.Cert.of_pem_cstruct1 |> 
    fun c -> (Certificate.asn_of_cert c).tbs_cert.pk_info |>
    function | PK.RSA pub -> pub | _ -> invalid_arg "No public key in certificate"

(* TODO *)
let split file = 
  let part = 
    let n = 0 in
    let name = Filename.(chop_extension @@ chop_extension @@ name_only file) in
    tmp @@ Printf.sprintf "%s.part.%i" name n in
  ignore @@ Sys.command @@ Printf.sprintf "cp %s %s" file part;
  [part]

let img_of_file ~key ~iv ~cert ~digest file = 
  let open Filename in
  let name = basename file in
  let bundle = tmp @@ Printf.sprintf "%s.tar.gz.enc" name in
  let user = try Sys.getenv "AWS_USER" with exn -> failwith "env variable AWS_USER not set" in
  let size = filesize file in
  let b_size = filesize bundle in
  let ec2_cert = None in (* TODO *)
  let ec2_pub_key = 
    let ec2_cert = match ec2_cert with 
      | Some c -> c
      (* TODO this file path *)
      | None -> "/usr/local/ec2/ec2-ami-tools-1.5.3/etc/ec2/amitools/cert-ec2.pem" in 
    pubkey_of_cert ec2_cert in
  let user_pub_key = pubkey_of_cert cert in 
  let ec2_enc_key = pub_enc ec2_pub_key key in
  let user_enc_key = pub_enc user_pub_key key in
  let ec2_enc_iv = pub_enc ec2_pub_key iv in
  let user_enc_iv = pub_enc user_pub_key iv in
  let parts = bundle |> split |> digest_parts in
  { name = name; user = user;
    img_type = "machine"; digest = digest;
    size = size; b_size = b_size; 
    ec2_enc_key = ec2_enc_key; user_enc_key = user_enc_key;
    ec2_enc_iv = ec2_enc_iv; user_enc_iv = user_enc_iv; 
    parts = parts; }
        
(*
Pipeline.execute: command = [/bin/bash -c 'openssl sha1 < /tmp/ec2-bundle-image-digest-pipe-10174 & tar -c -h -S --owner 0 --group 0 -C /home/jsp mymirage.img | tee /tmp/ec2-bundle-image-digest-pipe-10174 | gzip -9 | openssl enc -e -aes-128-cbc -K a3890552c945121fd49f0f08bfcc0a55 -iv 7e31f208a4211a2f46126b6b07d5e461 > ec2_tmp/mymirage.img.tar.gz.enc; echo ${PIPESTATUS[0]} > /tmp/image-bundle-pipeline-pipestatus-020140728-10174-1l636zs & echo ${PIPESTATUS[1]} > /tmp/image-bundle-pipeline-pipestatus-120140728-10174-1y9khin & echo ${PIPESTATUS[2]} > /tmp/image-bundle-pipeline-pipestatus-220140728-10174-1ys78ae & echo ${PIPESTATUS[3]} > /tmp/image-bundle-pipeline-pipestatus-320140728-10174-1p9bng1']

Pipeline.execute: output = [(stdin)= 37dc731c9f9c71de2310cb249c607726f3d8eabd]
 *)
let digest_pipe = tmp @@ Printf.sprintf "image-bundle-pipeline-%i" (Unix.getpid ())

let bundle f ~key ~iv () = 
  let open Unix in
  let dir = Filename.dirname f in
  let encrypted_dest = tmp @@ Printf.sprintf "%s.tar.gz.enc" (Filename.basename f) in
  mkfifo digest_pipe 0o666;
  let pipeline () = 
    let tar_expand = 
      let options = Printf.sprintf "-c -h -S --owner 0 --group 0 -C %s" dir
      and files = Filename.basename f in
      Printf.sprintf "tar %s %s" options files in
    let cmd = Printf.sprintf 
		"openssl sha1 < %s & %s | tee %s | gzip -9 | openssl enc -e -aes-128-cbc -K %s -iv %s > %s" 
		digest_pipe tar_expand digest_pipe key iv encrypted_dest in
    let ic = open_process_in cmd in
    let digest = input_line ic in
    Unix.close_process_in ic;
(*    print_string "BUNDLE COMMAND ";
    print_endline cmd;
    let result = format_ssl_output digest in
    print_string "DIGEST ";
    print_endline result;
    result in *)
    format_ssl_output digest in
  pipeline ()
	   
(* sha1 with private key*)
let sign keyfile i =
  let b = Buffer.create 1024 in (* closer to 2198 *)
  let oc = Xmlm.(make_output (`Buffer b) ~decl:false) in
  let out = Xmlm.output oc in
  let o_tree = 
    let frag = function
      | E (tag, childs) -> `El (tag, childs) 
      | D d -> `Data d in
    Xmlm.output_tree frag oc in
 let key = X509.PK.of_pem_cstruct1 @@ cs_of_file keyfile in
  out (`Dtd None);
  o_tree @@ tree_of_machconf mymachconf;
  out (`Dtd None);
  o_tree @@ tree_of_img i; 
(*  print_string "SIGNING ";
  print_endline @@ Buffer.contents b; *)
  Buffer.contents b |> Cstruct.of_string |>
    Nocrypto.Hash.SHA1.digest |>
    Nocrypto.RSA.PKCS1.sign ~key

(* TODO return (manifest location * [part locations]) *)
let manifest_of_file f ~key ~iv ~digest = 
  let img = img_of_file ~key ~iv ~cert:my_cert f ~digest in
  { version = manivers;
    bundler = bundler;
    machine_config = mymachconf;
    image = img;
    signature = match sign my_key img with 
		| Some s -> Cstruct.to_string s |> hex
		| None -> raise (Failure "couldn't sign manifest"); }

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
  let oc = open_out @@ tmp @@ Printf.sprintf "%s.manifest.xml" name in 
  let o = make_output (`Channel oc) in
  let out = output o in
  let frag = 
(*    let tag n = ("",n), [] in *)
    function
    | E (tag, childs) -> `El (tag, childs) 
    | D d -> `Data d in
  let o_tree = Xmlm.output_tree frag o in
  out (`Dtd None);
  o_tree m;
  close_out oc

let test_img = tmp "mymirage.img"

let clean_up () = 
  (* TODO delete the tar.gz.enc file *)
  let to_delete = [digest_pipe;] in
  ignore @@ List.map (fun s -> Sys.command @@ Printf.sprintf "rm %s" s) to_delete 

(* img -> ~cert -> ~key -> ?ec2_cert -> (xml_manifest, [parts_filenames] *)
let bundle_img f = 
  Nocrypto.Rng.reseed (Cstruct.of_string "\001\002\003\004");
  let gen_key () = Nocrypto.Rng.generate 16 |> Cstruct.to_string |> hex in
  let key = gen_key ()
  and iv = gen_key ()  in 
  let digest = bundle ~key ~iv f () in
  manifest_of_file f ~key ~iv ~digest |> 
    tree_of_manifest |>
    create_manifest (Filename.basename f);
  clean_up ()
