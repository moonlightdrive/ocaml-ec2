open S3

let _ = 
  Lwt_main.run (API.get "a-bucket-of-hippos" "mirage.image.manifest.xml")
