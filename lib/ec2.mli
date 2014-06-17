module EC2_t :
  sig
    type time = string
    type successful = bool
    type create_snapshot = {
      snapshot : string;
      volume : string;
      status : string;
      start_time : string;
      progress : string;
      owner : string;
      size : string;
      encrypted : bool;
      description : string;
    }
    type console_output = {
      instance : string;
      timestamp : time;
      output : string; (* base64 encoded *)
    }
    type instance_state = { code : int; name : string; }
    type running_instance = {
      id : string;
      image : string;
      state : instance_state;
      private_dns : string;
      public_dns : string;
      reason : string;
      key_name : string;
      ami_launch_index : string;
      instance_type : string;
      launch_time : time;
      kernel : string;
    }
    type run_instances = {
      reservation : string;
      owner : string;
      instances : running_instance list;
      requester : string;
    }
    type instance_state_change = {
      id : string;
      current : instance_state;
      previous : instance_state;
    }
    type region = { name : string; endpoint : string; }
    type describe_regions = region list
  end
module EC2_x :
  sig
    val _member : string -> Ezxmlm.nodes -> string
    val success : Ezxmlm.nodes -> bool
    val dereg_img_of_string : Ezxmlm.nodes -> bool
    val reg_img_of_string : Ezxmlm.nodes -> string
    val desc_regions_of_string : Ezxmlm.nodes -> EC2_t.describe_regions
  end
module Monad : sig
  type 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val return : 'a -> 'a t
end
module API :
  sig
    val realize_body : (string * string) list -> Cohttp_lwt_body.t
    val realize_headers :
      Cohttp.Code.meth ->
      'a -> params:(string * string) list -> region:string -> Cohttp.Header.t
    val make_req :
      Cohttp.Code.meth ->
      Cohttp.Header.t ->
      Cohttp_lwt_body.t ->
      Uri.t -> (Cohttp.Response.t * Cohttp_lwt_body.t) Lwt.t
    val get :
      string ->
      (Ezxmlm.nodes -> 'a) ->
      ?region:string -> params:(string * string) list -> 'a Lwt.t
    val post :
      string ->
      (Ezxmlm.nodes -> 'a) ->
      ?region:string -> params:(string * string) list -> 'a Lwt.t
  end
module AMI :
  sig
   (* val deregister_image : EC2_t.img_id -> ?region:string -> bool Lwt.t*)
    val deregister_image : string -> ?region:string -> bool Lwt.t
    val register_image : string -> ?image:string -> ?region:string -> string Lwt.t
  end
module Instances :
  sig
(*    val describe_status : ?ids:string list -> ?all:bool -> ?region:string -> ___ Lwt.t *)
    val get_console_output : string -> ?region:string -> EC2_t.console_output Lwt.t
    val run : ?min:int -> ?max:int -> ?instance:string -> ?zone:string -> ?kernel:string -> string -> ?region:string -> EC2_t.run_instances Lwt.t
    val start : string list -> ?region:string -> EC2_t.instance_state_change list Lwt.t
    val stop : ?force:bool -> string list -> ?region:string -> EC2_t.instance_state_change list Lwt.t
    val terminate : string list -> ?region:string -> EC2_t.instance_state_change list Lwt.t
  end										  
module Regions :
  sig
    val describe : ?region:string -> EC2_t.describe_regions Lwt.t
  end
  
