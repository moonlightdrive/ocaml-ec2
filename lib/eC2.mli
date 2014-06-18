module Monad : sig
  type 'a t
  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val run : 'a t -> 'a Lwt.t
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
end
module AMI :
  sig
   (* val deregister_image : EC2_t.img_id -> ?region:string -> bool Lwt.t*)
    val deregister_image : string -> ?region:string -> unit -> bool Monad.t
    val register_image : string -> ?image:string -> ?region:string -> unit -> string Monad.t	  
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
  
