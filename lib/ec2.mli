val service : string
val version : string
val key : string -> string
val iam_secret : string
val iam_access : string
module EC2_t :
  sig
    type time = string
    type successful = bool
    type register_img = string
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
      output : string;
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
module Time :
  sig
    val date_yymmdd : Core.Time0.t -> string
    val date_time : Core.Time0.t -> string
    val now_utc : Core.Time0.t
  end
module Hash :
  sig
    val hex_encode : string -> string
    val sha256 : ?k:string -> string -> string
    val hex_hash : ?k:string -> string -> string
  end
module URI : sig val host : string -> string val base : string -> Uri.t end
module Field :
  sig
    val to_string : string * string -> string
    val query_string : (string * string) list -> string
    val number_fields :
      (int -> 'a, unit, string) format -> 'b list -> ('a * 'b) list
  end
module Signature :
  sig
    val signing_algorithm : string
    val v4_req : string
    val content_type : string
    val signed_headers : string
    val canonical_headers : timestamp:Core.Time0.t -> string -> string
    val canonical_request :
      Cohttp.Code.meth ->
      timestamp:Core.Time0.t ->
      host:string ->
      ?uri:string -> ?query:string -> ?payload:string -> unit -> string
    val credential_scope : Core.Time0.t -> string -> string
    val str_to_sign :
      timestamp:Core.Time0.t -> cred_scope:string -> req:string -> string
    val signature :
      secret:string ->
      timestamp:Core.Time0.t -> region:string -> string -> string
  end
module API :
  sig
    val param_list :
      params:(string * string) list -> string -> (string * string) list
    val realize_body : (string * string) list -> Cohttp_lwt_body.t
    val realize_headers :
      Cohttp.Code.meth ->
      'a -> params:(string * string) list -> region:string -> Cohttp.Header.t
    val make_req :
      Cohttp.Code.meth ->
      Cohttp.Header.t ->
      Cohttp_lwt_body.t ->
      Uri.t -> (Cohttp.Response.t * Cohttp_lwt_body.t) Lwt.t
    val handle_response :
      string -> (Ezxmlm.nodes -> 'a) -> 'b * Cohttp_lwt_body.t -> 'a Lwt.t
    val verb :
      Cohttp.Code.meth ->
      string ->
      (Ezxmlm.nodes -> 'a) ->
      ?region:string -> params:(string * string) list -> 'a Lwt.t
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
    val deregister_image : string -> ?region:string -> bool Lwt.t
    val register_image : string -> ?image:string -> ?region:string -> string Lwt.t
  end
module Regions :
  sig
    val describe : ?region:string -> EC2_t.describe_regions Lwt.t
  end
