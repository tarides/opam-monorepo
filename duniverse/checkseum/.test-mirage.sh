#!/bin/sh

set -ex

opam install -y mirage
(cd mirage && mirage configure -t unix && make depends && mirage build && ./checkseum_test && mirage clean && cd ..) || exit 1
(cd mirage && mirage configure -t hvt && make depends && mirage build && mirage clean && cd ..) || exit 1
if [ $(uname -m) = "amd64" ] || [ $(uname -m) = "x86_64" ]; then
    (cd mirage && mirage configure -t xen && make depend && mirage build && mirage clean && cd ..) || exit 1
fi
