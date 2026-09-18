#!/usr/bin/env bash

set -e
set -u
set -o pipefail

if test "$(dirname "$0")" != "." ; then
  echo "usage: ./test.sh"
  exit 1
fi

export OPAMROOT=./.opam-root

printf "Enter the path to the opam binary without builtin patch: "
read -r OLD_OPAM
printf "Enter the path to the opam binary with builtin patch: "
read -r NEW_OPAM

echo
echo '## Getting the list of packages with patches'
echo
rm -f pkgs-with-patches

rm -rf ./opam-repository
git clone --depth=1 https://github.com/ocaml/opam-repository.git
pushd ./opam-repository/packages > /dev/null
grep -rl '^patches:' . | grep '/opam$' | cut -d/ -f3 >> ../../pkgs-with-patches
popd > /dev/null

rm -rf ./opam-repository-archive
git clone --depth=1 https://github.com/ocaml/opam-repository-archive.git
pushd ./opam-repository-archive/packages > /dev/null
grep -rl '^patches:' . | grep '/opam$' | cut -d/ -f3 >> ../../pkgs-with-patches
popd > /dev/null

NB_OF_PKGS=$(cat ./pkgs-with-patches | wc -l)
echo "Number of packages to process: ${NB_OF_PKGS}"

rm_faulty_pkg_artefacts() {
  rm -rf ./ocaml-variants.4.10.0+nnpcheck/.git
  rm ./coq.8.7.1+1/test-suite/bugs/closed/4722/tata
  rm ./coq.8.7.1+2/test-suite/bugs/closed/4722/tata
  rm ./opa-base.1.1.0+4263/ocamllib/libbase/default.trx
}

echo
echo "## Extract and apply patches using ${OLD_OPAM}"
echo

rm -rf ./tmp ./old
mkdir ./tmp
pushd ./tmp > /dev/null
"${OLD_OPAM}" init --bare --no-setup --no-opamrc ../opam-repository
"${OLD_OPAM}" repository add --set-default archive ../opam-repository-archive
cat ../pkgs-with-patches | time -p xargs -n1 "${OLD_OPAM}" source > ../old.log 2>&1 || true
rm_faulty_pkg_artefacts
rm -rf "${OPAMROOT}"
popd > /dev/null
mv ./tmp ./old

echo
echo "## Extract and apply patches using ${NEW_OPAM}"
echo

rm -rf ./tmp ./new
mkdir ./tmp
pushd ./tmp > /dev/null
"${NEW_OPAM}" init --bare --no-setup --no-opamrc ../opam-repository
"${NEW_OPAM}" repository add --set-default archive ../opam-repository-archive
cat ../pkgs-with-patches | time -p xargs -n1 "${NEW_OPAM}" source > ../new.log 2>&1 || true
rm_faulty_pkg_artefacts
rm -rf "${OPAMROOT}"
popd > /dev/null
mv ./tmp ./new

echo >> ./test.log
echo >> ./test.log
echo "Run on the $(date):" >> ./test.log
echo "Time of old run:" >> ./test.log
tail -n3 ./old.log >> ./test.log
echo "Time of new run:" >> ./test.log
tail -n3 ./new.log >> ./test.log
echo "Diff:" >> ./test.log
diff -qr ./old ./new >> ./test.log 2>&1 || true
echo "Done" >> ./test.log

echo "Done."
