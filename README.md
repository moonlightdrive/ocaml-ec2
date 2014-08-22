ocaml-ec2
=========
ocaml-ec2 provides OCaml bindings to Amazon's Elastic Compute Cloud. This library is incomplete and a bit rough around the edges.

Some examples of usage can be found in the `examples` directory.

# Installation
``` 
$ ./configure 
# or, if you want support for registering your own AMIs:
# $ ./configure --enable-ami-tools
$ make
$ make install
```

# Setup
You'll need to export your access and secret keys e.g. 
```
$ export AWS_ACCESS_KEY=AKIAIOSFODNN7EXAMPLE
$ export AWS_SECRET_KEY=wJalrXUtnFEMI/K7MDENG+bPxRfiCYEXAMPLEKEY
$ export REGION=us-east-1 # optional
$ export AWS_USER=01234567890 # optional; 12-digit account number without dashes
```