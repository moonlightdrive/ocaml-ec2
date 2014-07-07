module Monad : sig
  type 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val return : 'a -> 'a t
  val run : 'a t -> 'a Lwt.t
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
end

module AMI : sig
  val create_image :
    name:string ->
    ?description:string ->
    EC2_t.InstanceID.t ->
    ?region:EC2_t.region_name -> unit -> EC2_t.ImageID.t Monad.t
  val deregister_image : 
    EC2_t.ImageID.t -> 
    ?region:EC2_t.region_name-> unit -> bool Monad.t
  val register_image : 
    name:string -> 
    ?img_path:string -> 
    ?desc:string ->
    ?arch:string -> 
    ?region:EC2_t.region_name -> unit -> EC2_t.ImageID.t Monad.t	  
end
module EBS : sig
  val create_snapshot : 
    EC2_t.VolumeID.t -> 
    ?description:string -> 
    ?region:EC2_t.region_name -> unit -> EC2_t.create_snapshot Monad.t
  val delete_volume : 
    EC2_t.VolumeID.t -> 
    ?region:EC2_t.region_name -> unit -> bool Monad.t
end 
module Instances :
sig
  (*    val describe_status : ?ids:string list -> ?all:bool -> ?region:EC2_t.region_name -> ___ Lwt.t *)
  val get_console_output : 
    EC2_t.InstanceID.t -> 
    ?region:EC2_t.region_name -> unit -> EC2_t.console_output Monad.t
  val run : 
    ?min:int -> 
    ?max:int -> 
    ?instance:string -> 
    ?zone:string -> 
    ?kernel:string -> 
    EC2_t.ImageID.t -> 
    ?region:EC2_t.region_name -> unit -> EC2_t.run_instances Monad.t
  val start : 
    EC2_t.InstanceID.t list -> 
    ?region:EC2_t.region_name -> unit -> EC2_t.instance_state_change list Monad.t
  val stop : 
    ?force:bool -> 
    EC2_t.InstanceID.t list -> 
    ?region:EC2_t.region_name-> unit -> EC2_t.instance_state_change list Monad.t
  val terminate : 
    EC2_t.InstanceID.t list -> 
    ?region:EC2_t.region_name -> unit -> EC2_t.instance_state_change list Monad.t
end	
module KeyPairs : sig
  val describe :
    ?names:string list -> 
    ?filters:(string * string list) list ->
    ?region:EC2_t.region_name -> unit -> EC2_t.key_pair list Monad.t
end									  
module Regions : sig
  val describe : 
    ?regions:EC2_t.region_name list ->
    ?filters:(string * string list) list ->
    ?region:EC2_t.region_name -> unit -> EC2_t.region list Monad.t
end
  
