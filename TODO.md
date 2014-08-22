by aug 25
---------
- [ ] seed the random generator (oops)
- [x] oasis
  - [x] flag for installing ec2.ami
  - [x] set up tests
- [ ] implement Regions functions
  - [ ] which is just DescribeAvailabilityZones :)

upcoming
--------
- [ ] fix to_hex (currently doing embarrassing string manip on Cstruct.hexdump)
- [ ] would be nice to have a monad.mli if possible
- [ ] move common functions in eC2.ml & s3.ml to utils.ml
- [ ] ec2 commands: insuring idempotency
- [ ] implement remaining EC2 API

eventually
----------
- [ ] deal with s3 errors
- [ ] determine the proper way of getting OASIS's data_dir