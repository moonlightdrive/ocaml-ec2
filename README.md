ocaml-ec2
=========
ocaml-ec2 provides OCaml bindings to Amazon's Elastic Compute Cloud. This library is incomplete and a bit rough around the edges.

Some examples of usage can be found in the `lib_test` directory.

# Installation
``` 
$ ./configure
$ make
$ make install
```

# Setup
You'll need to export your access and secret keys e.g. 
```
$ export AWS_ACCESS_KEY=AKIAIOSFODNN7EXAMPLE
$ export AWS_SECRET_KEY=wJalrXUtnFEMI/K7MDENG+bPxRfiCYEXAMPLEKEY
$ export REGION=us-east-1 # optional
```

# A Utop Session
```
# #require "ec2";;
# open EC2;;
# open EC2_t;; 
# Regions.describe () |> Monad.run;;
- : EC2_t.region list =
[{EC2_t.name = EC2_t.EU_WEST_1; endpoint = "ec2.eu-west-1.amazonaws.com"};
 {EC2_t.name = EC2_t.SA_EAST_1; endpoint = "ec2.sa-east-1.amazonaws.com"};
 {EC2_t.name = EC2_t.US_EAST_1; endpoint = "ec2.us-east-1.amazonaws.com"};
 {EC2_t.name = EC2_t.AP_NORTHEAST_1;
  endpoint = "ec2.ap-northeast-1.amazonaws.com"};
 {EC2_t.name = EC2_t.US_WEST_2; endpoint = "ec2.us-west-2.amazonaws.com"};
 {EC2_t.name = EC2_t.US_WEST_1; endpoint = "ec2.us-west-1.amazonaws.com"};
 {EC2_t.name = EC2_t.AP_SOUTHEAST_1;
  endpoint = "ec2.ap-southeast-1.amazonaws.com"};
 {EC2_t.name = EC2_t.AP_SOUTHEAST_2;
  endpoint = "ec2.ap-southeast-2.amazonaws.com"}]
# Regions.describe filters:["region-name", ["*west*";"*northeast*"]] () |> Monad.run;;
- : region list =
[{name = EU_WEST_1; endpoint = "ec2.eu-west-1.amazonaws.com"};
 {name = AP_NORTHEAST_1; endpoint = "ec2.ap-northeast-1.amazonaws.com"};
 {name = US_WEST_2; endpoint = "ec2.us-west-2.amazonaws.com"};
 {name = US_WEST_1; endpoint = "ec2.us-west-1.amazonaws.com"}]
```