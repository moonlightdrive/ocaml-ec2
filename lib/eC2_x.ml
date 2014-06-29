open Ezxmlm
open EC2_t

let string_of_member name xml = data_to_string (member name xml)

let success xml = bool_of_string (string_of_member "return" xml)


let create_img_of_string x = string_of_member "imageId" x |> ImageID.of_string

let dereg_img_of_string = success

let reg_img_of_string x = string_of_member "imageId" x |> ImageID.of_string

let create_snap_of_string x =
  { snapshot = member "snapshotId" x |> data_to_string;
    volume = string_of_member "volumeId" x;
    status = string_of_member "status" x;
    start_time = string_of_member "startTime" x; (* string -> time? *)
    progress = string_of_member "progress" x;
    owner = string_of_member "ownerId" x;
    size = string_of_member "volumeSize" x;
    encrypted = string_of_member "encrypted" x |> bool_of_string;
    description = string_of_member "description" x; }
    
let delete_vol_of_string = success

let console_output_of_string x =
  { instance = string_of_member "instanceId" x;
    timestamp = string_of_member "timestamp" x;
    output = string_of_member "output" x; }

let instance_state_of_string x = 
  { code = string_of_member "code" x |> int_of_string;
    name = string_of_member "name" x }

let group_of_string item = { id = string_of_member "groupId" item;
			     name = string_of_member "groupName" item; } 

let running_instances_of_string item = 
  { id = string_of_member "instanceId" item;
    image = string_of_member "imageId" item;
    state = member "instanceState" item |> instance_state_of_string;
    private_dns = string_of_member "privateDnsName" item;
    public_dns = string_of_member "dnsName" item;
    reason = string_of_member "reason" item;
    key_name = string_of_member "keyName" item;
    ami_launch_index = string_of_member "amiLaunchIndex" item;
    (*product_codes = member "productCodes" item |> members "item" |> smth_of_string *)
    instance_type = string_of_member "instanceType" item;
    launch_time = string_of_member "launchTime" item;
    kernel = string_of_member "kernelId" item }  

let run_instances_of_string x =
  { reservation = string_of_member "reservationId" x;
    owner = string_of_member "ownerId" x;
    security_groups = member "groupSet" x |> members "item" |>
			 List.map group_of_string;
    instances = member "instancesSet" x |> members "item" 
		|> List.map running_instances_of_string;
    requester = string_of_member "requesterId" x }					 

let instance_state_change_of_string x = 
  { id = string_of_member "instanceId" x;
    current = member "currentState" x |> instance_state_of_string;
    previous = member "previousState" x |> instance_state_of_string; }

let start_instances_of_string x =
  members "item" x |>
    List.map instance_state_change_of_string

let stop_instances_of_string x =
  members "item" x |>
    List.map instance_state_change_of_string

let terminate_instances_of_string x =
  members "item" x |>
    List.map instance_state_change_of_string
 
(* KeyPairs *)
let desc_keys_of_string x = 
  member "keySet" x |> members "item" |>
    List.map (fun i -> { name = string_of_member "keyName" i;
			 fingerprint = string_of_member "keyFingerprint" i; })

(* Regions *)
let desc_regions_of_string x =
  member "regionInfo" x |> members "item" |>
    List.map (fun i -> { name = string_of_member "regionName" i |> region_of_string;
			 endpoint = string_of_member "regionEndpoint" i; })
