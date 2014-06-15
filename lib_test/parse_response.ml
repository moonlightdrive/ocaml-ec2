open Ezxmlm

let (_,deregister_img_resp) = from_string "<DeregisterImageResponse xmlns=\"http://ec2.amazonaws.com/doc/2014-05-01/\"><requestId>59dbff89-35bd-4eac-99ed-be587EXAMPLE</requestId><return>true</return></DeregisterImageResponse>"

let t = member "DeregisterImageResponse" deregister_img_resp |> member "return" |> data_to_string

let _ = print_endline t
