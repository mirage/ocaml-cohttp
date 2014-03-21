OPAM_DEPENDS="lwt async async_ssl ssl uri re"

case "$OCAML_VERSION,$OPAM_VERSION" in
4.00.1,1.0.0) ppa=avsm/ocaml40+opam10 ;;
4.00.1,1.1.0) ppa=avsm/ocaml40+opam11 ;;
4.01.0,1.0.0) ppa=avsm/ocaml41+opam10 ;;
4.01.0,1.1.0) ppa=avsm/ocaml41+opam11 ;;
*) echo Unknown $OCAML_VERSION,$OPAM_VERSION; exit 1 ;;
esac

echo "yes" | sudo add-apt-repository ppa:$ppa
sudo apt-get update -qq
sudo apt-get install -qq ocaml ocaml-native-compilers camlp4-extra opam time libssl-dev

export OPAMYES=1
export OPAMVERBOSE=1
echo OCaml version
ocaml -version
echo OPAM versions
opam --version
opam --git-version

opam init git://github.com/ocaml/opam-repository >/dev/null 2>&1
opam install ${OPAM_DEPENDS}

eval `opam config env`
make NETTESTS=--enable-nettests
make test
make clean
# Test out some upstream users of Cohttp
opam pin cohttp .

unset OPAMVERBOSE
opam install github
opam install cowabloga
opam install mirage-www

case "$OCAML_VERSION" in
4.01.0)
  opam install irminsule ;;
*) echo Skipping irminsule ;;
esac
