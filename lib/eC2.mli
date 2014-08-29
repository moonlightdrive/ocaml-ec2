(* 
  module Monad : sig
  type 'a t
  type 'a signal
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val return : 'a -> 'a t
  val run : 'a t -> 'a Lwt.t
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
end 
 *)

(* For testing purposes. No need to call these directly *)
module API : sig
  val handle_response : string -> (Ezxmlm.nodes -> 'a) -> 
			Cohttp.Response.t * Cohttp_lwt_body.t -> 
			'a Monad.signal Lwt.t
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
    ?region:EC2_t.region_name -> unit -> EC2_t.Create_snapshot.t Monad.t
  val delete_volume : 
    EC2_t.VolumeID.t -> 
    ?region:EC2_t.region_name -> unit -> bool Monad.t
end 
module Instances :
sig
  (*    val describe_status : ?ids:string list -> ?all:bool -> ?region:EC2_t.region_name -> ___ Lwt.t *)
  val get_console_output : 
    EC2_t.InstanceID.t -> 
    ?region:EC2_t.region_name -> unit -> EC2_t.Console_output.t Monad.t
  val run : 
    ?min:int -> 
    ?max:int -> 
    ?instance:string -> 
    ?zone:string -> 
    ?kernel:string -> 
    EC2_t.ImageID.t -> 
    ?region:EC2_t.region_name -> unit -> EC2_t.Run_instances.t Monad.t
  val start : 
    EC2_t.InstanceID.t list -> 
    ?region:EC2_t.region_name -> unit -> EC2_t.Instance_state_change.t list Monad.t
  val stop : 
    ?force:bool -> 
    EC2_t.InstanceID.t list -> 
    ?region:EC2_t.region_name-> unit -> EC2_t.Instance_state_change.t list Monad.t
  val terminate : 
    EC2_t.InstanceID.t list -> 
    ?region:EC2_t.region_name -> unit -> EC2_t.Instance_state_change.t list Monad.t
end	
module KeyPairs : sig
  type name = string
  val delete : name -> ?region:EC2_t.region_name -> unit -> bool Monad.t
  val describe :
    ?ns:name list -> 
    ?filters:(string * string list) list ->
    ?region:EC2_t.region_name -> unit -> EC2_t.Key_pair.t list Monad.t
end									  
module Regions : sig
  val describe : 
    ?regions:EC2_t.region_name list ->
    ?filters:(string * string list) list ->
    ?region:EC2_t.region_name -> unit -> EC2_t.Region.t list Monad.t
  val describe_zones :
    ?zones:string list ->
    ?filters:(string * string list) list ->
    ?region:EC2_t.region_name -> unit -> EC2_t.Zone.t list Monad.t
end
  
