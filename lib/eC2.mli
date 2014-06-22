module AMI : sig
  (* val create_image : *)
   (* val deregister_image : EC2_t.img_id -> ?region:string -> bool Lwt.t*)
    val deregister_image : string -> ?region:string -> unit -> bool Monad.t
    val register_image : string -> ?image:string -> ?region:string -> unit -> string Monad.t	  
end
module EBS : sig
  val create_snapshot : ?description:string -> string -> ?region:string -> unit -> EC2_t.create_snapshot Monad.t
  val delete_volume : string -> ?region:string -> unit -> bool Monad.t
end 
module Instances :
sig
  (*    val describe_status : ?ids:string list -> ?all:bool -> ?region:string -> ___ Lwt.t *)
  val get_console_output : string -> ?region:string -> unit -> EC2_t.console_output Monad.t
  val run : ?min:int -> ?max:int -> ?instance:string -> ?zone:string -> ?kernel:string -> string -> ?region:string -> unit -> EC2_t.run_instances Monad.t
  val start : string list -> ?region:string -> unit -> EC2_t.instance_state_change list Monad.t
  val stop : ?force:bool -> string list -> ?region:string-> unit -> EC2_t.instance_state_change list Monad.t
  val terminate : string list -> ?region:string -> unit -> EC2_t.instance_state_change list Monad.t
end										  
module Regions : sig
  val describe : ?region:string -> unit -> EC2_t.describe_regions Monad.t
end
  
