FROM ocaml/opam:alpine_ocaml-4.03.0
COPY . /home/opam/src
RUN sudo chown -R opam /home/opam/src
RUN opam pin add -n decompress /home/opam/src
RUN opam depext -uivj 3 decompress
