val bundle_img : key:string -> cert:string -> ?user:string -> string -> string * string list

val upload : string * string list -> bucket:string -> unit Lwt.t list
