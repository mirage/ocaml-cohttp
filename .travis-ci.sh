OPAM_DEPENDS="lwt stringext ssl uri re conduit sexplib fieldslib ipaddr cstruct js_of_ocaml cmdliner base64"

case "$OCAML_VERSION,$OPAM_VERSION" in
4.01.0,1.2.0) ppa=avsm/ocaml41+opam12; pin="add" ;;
4.02.0,1.2.0) ppa=avsm/ocaml42+opam12; pin="add" ;;
*) echo Unknown $OCAML_VERSION,$OPAM_VERSION; exit 1 ;;
esac

git config --global user.email "you@example.com"
git config --global user.name "Your Name"
echo "yes" | sudo add-apt-repository ppa:$ppa
sudo apt-get update -qq
sudo apt-get install -qq ocaml ocaml-native-compilers camlp4-extra opam time

export OPAMYES=1
echo OCaml version
ocaml -version
echo OPAM versions
opam --version
opam --git-version

opam init
opam remote add mirage-dev git://github.com/mirage/mirage-dev
sudo apt-get install -qq `opam install -e ubuntu ${OPAM_DEPENDS}`
opam pin add cohttp .
opam install -v core_kernel
opam install ${OPAM_DEPENDS}
eval `opam config env`
make NETTESTS=--enable-nettests
make test
make clean
export OPAMJOBS=2
# Test out some upstream users of Cohttp
opam install github cowabloga mirage
git clone git://github.com/mirage/mirage-www
cd mirage-www && make configure && make depend && make build 
