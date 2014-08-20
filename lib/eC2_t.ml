(* 
 * Copyright (c) 2014, Jyotsna Prakash <jyotsna.prakash@gmail.com>
 *
 * Permission to use, copy, modify, and/or distribute this software for any 
 * purpose with or without fee is hereby granted, provinded that the above 
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES 
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 *)

module type ID = sig
  type t
  val of_string : string -> t
  val to_string : t -> string
end 

module ID = struct
  type t = string
  let of_string x = x
  let to_string x = x
end

module ImageID = ID 
module InstanceID = ID
module VolumeID = ID	
 
type region_name = 
    US_EAST_1 | US_WEST_2 | US_WEST_1 |
    EU_WEST_1 | 
    AP_SOUTHEAST_1 | AP_SOUTHEAST_2 | AP_NORTHEAST_1 |
    SA_EAST_1

let region_of_string = function
  | "us-east-1" -> US_EAST_1 
  | "us-west-1" -> US_WEST_1 
  | "us-west-2" -> US_WEST_2
  | "eu-west-1" -> EU_WEST_1
  | "ap-southeast-1" -> AP_SOUTHEAST_1 
  | "ap-southeast-2" -> AP_SOUTHEAST_2
  | "ap-northeast-1" -> AP_NORTHEAST_1
  | "sa-east-1" -> SA_EAST_1
  | s -> invalid_arg @@ Printf.sprintf "region_of_string: %s" s

let string_of_region = function
  | US_EAST_1 -> "us-east-1"
  | US_WEST_1 -> "us-west-1"
  | US_WEST_2 -> "us-west-2"
  | EU_WEST_1 -> "eu-west-1"
  | AP_SOUTHEAST_1 -> "ap-southeast-1"
  | AP_SOUTHEAST_2 -> "ap-southeast-2"
  | AP_NORTHEAST_1 -> "ap-nourtheast-1"
  | SA_EAST_1 -> "sa-east-1"

type time = string
	      
type successful = bool

module Create_snapshot = struct
  type t = { snapshot: string;
	     volume: string;
	     status: string; (* pending|completed|error *)
	     start_time: string;
	     progress: string;
	     owner: string;
	     size: string;
	     encrypted: bool;
	     description: string;
	   }
end
module Console_output = struct
  type t = { instance: string;
	     timestamp: time;
	     output: string;
	   }
end
module Instance_state = struct
  (* name: pending|running|shutting-down... code: 16-bit unsigned
   * see http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-ItemType-InstanceStateType.html *)
  type t = { code: int; name: string; }
end
(* Describes a security group *) 
module Group_item = struct
  type t  = { id: string; name: string; }
end
module Running_instance = struct
  type t = { id: string;
	     image: string;
	     (*	  state: instance_state;
			  private_dns: string;
			  public_dns: string;
			  reason: string;
			  key_name: string;
			  ami_launch_index: string; *)
	     (*product_codes: productCodesSetItemType*)
	     instance_type: string;
	     launch_time: time;
	     (*placement:*)
	     kernel: string;
	   (* TODO there are more fields! *)
	   }
end			  
module Run_instances = struct
  type t = { reservation: string;
	     owner: string;
	     (*	security_groups: group_item list; *)
	     instances: Running_instance.t list; 
	   }
end
module Instance_state_change = struct
  type t = { id: string; 
	     current: Instance_state.t;
	     previous: Instance_state.t;
	   }
end
module Key_pair = struct
  type t  = { name: string; fingerprint: string; }
end
module Region = struct
  type t = { name: region_name; endpoint: string; }
end
