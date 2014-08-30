by sep 5
--------
- [ ] eC2_t: having `Create_key_pair.t` and `Key_pair.t` is ugly... would prefer `Key_pair.created` and `Key_pair.description`
- [ ] ec2 commands: ensuring idempotency
- [ ] add (error) `Response` XML parsing test
- [ ] implement Instances functions

upcoming
--------
- [ ] fix `to_hex` (currently doing embarrassing string manip on Cstruct.hexdump)
- [ ] would be nice to have a monad.mli if possible
- [ ] turn travis back on
- [ ] cleanup files when things fail!
- [ ] XML: fail gracefully (?) when Amazon promises us a field that we don't actually get

eventually
----------
- [ ] deal with s3 errors
- [ ] determine the proper way of getting OASIS's data_dir
- [ ] what if we could generate eC2_x.ml & eC2_t.ml{,i} from the wdsl?!
- [ ] eC2.handle_response logic is getting a little confusing