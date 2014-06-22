open S3

let _ = 
  Lwt_main.run (API.get "my-bucket!!" "mirage.image.manifest.xml")
