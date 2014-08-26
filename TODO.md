by aug 29
---------
- [ ] move common functions in eC2.ml & s3.ml to utils.ml
- [ ] XML parsing 
  - [ ] improve errors eg "id in ParseInstancesResponse"
  - [ ] handle lack of field that Amazon promised would be returned
- [ ] implement KeyPairs functions

upcoming
--------
- [ ] fix `to_hex` (currently doing embarrassing string manip on Cstruct.hexdump)
- [ ] would be nice to have a monad.mli if possible
- [ ] ec2 commands: insuring idempotency
- [ ] implement remaining EC2 API
- [ ] turn travis back on
- [ ] cleanup files when things fail!

eventually
----------
- [ ] deal with s3 errors
- [ ] determine the proper way of getting OASIS's data_dir
- [ ] what if we could generate eC2_x.ml & eC2_t.ml{,i} from the wdsl?!