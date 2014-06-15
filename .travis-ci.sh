OPAM_DEPENDS="lwt cohttp ezxmlm uri cryptokit core_extended threads"

ppa=avsm/ocaml41+opam11
echo "yes" | sudo add-apt-repository ppa:$ppa
sudo apt-get update -qq
sudo apt-get install -qq ocaml ocaml-native-compilers camlp4-extra opam

export OPAMYES=1
export OPAMVERBOSE=1

opam init
#opam pin?
opam install $(OPAM_DEPENDS)
eval `opam config env`
make
