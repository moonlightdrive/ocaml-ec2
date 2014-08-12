val bundle_img : key:string -> cert:string -> ?ec2_cert:string -> ?user:string -> string -> (string * string list) Lwt.t

val upload : ?region:EC2_t.region_name -> string * string list -> bucket:string -> unit Lwt.t list
