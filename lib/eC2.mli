module AMI : sig
  (* val create_image : *)
    val deregister_image : 
      EC2_t.ImageID.t -> 
      ?region:string -> unit -> bool Monad.t
    val register_image : 
      name:string -> 
      ?img_path:string -> 
      ?region:string -> unit -> EC2_t.ImageID.t Monad.t	  
end
module EBS : sig
  val create_snapshot : 
    EC2_t.VolumeID.t -> 
    ?description:string -> 
    ?region:string -> unit -> EC2_t.create_snapshot Monad.t
  val delete_volume : 
    EC2_t.VolumeID.t -> 
    ?region:string -> unit -> bool Monad.t
end 
module Instances :
sig
  (*    val describe_status : ?ids:string list -> ?all:bool -> ?region:string -> ___ Lwt.t *)
  val get_console_output : 
    EC2_t.InstanceID.t -> 
    ?region:string -> unit -> EC2_t.console_output Monad.t
  val run : 
    ?min:int -> 
    ?max:int -> 
    ?instance:string -> 
    ?zone:string -> 
    ?kernel:string -> 
    EC2_t.ImageID.t -> 
    ?region:string -> unit -> EC2_t.run_instances Monad.t
  val start : 
    EC2_t.InstanceID.t list -> 
    ?region:string -> unit -> EC2_t.instance_state_change list Monad.t
  val stop : 
    ?force:bool -> 
    EC2_t.InstanceID.t list -> 
    ?region:string-> unit -> EC2_t.instance_state_change list Monad.t
  val terminate : 
    EC2_t.InstanceID.t list -> 
    ?region:string -> unit -> EC2_t.instance_state_change list Monad.t
end										  
module Regions : sig
  val describe : 
    ?region:string -> unit -> EC2_t.region list Monad.t
end
  
