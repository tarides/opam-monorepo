Test cases to check Coq's flag setting is correct:

  $ cat > dune-project <<EOF
  > (lang dune 2.8)
  > (using coq 0.3)
  > EOF

  $ cat > foo.v <<EOF
  > Definition t := 3.
  > EOF

Test case: default flags

  $ cat > dune <<EOF
  > (coq.theory
  >  (name foo))
  > EOF

  $ dune build foo.vo && tail -n 1 _build/log | sed 's/(cd .*coqc/coqc/' | sed 's/$ //'
  coqc -q -w -deprecated-native-compiler-option -w -native-compiler-disabled -native-compiler ondemand -R . foo foo.v)

TC: :standard

  $ cat > dune <<EOF
  > (coq.theory
  >  (name foo)
  >  (flags :standard))
  > EOF

  $ rm _build/default/foo.vo
  $ dune build foo.vo && tail -n 1 _build/log | sed 's/(cd .*coqc/coqc/' | sed 's/$ //'
  coqc -q -w -deprecated-native-compiler-option -w -native-compiler-disabled -native-compiler ondemand -R . foo foo.v)

TC: override :standard

  $ cat > dune <<EOF
  > (coq.theory
  >  (name foo)
  >  (flags ))
  > EOF

  $ dune build foo.vo && tail -n 1 _build/log | sed 's/(cd .*coqc/coqc/' | sed 's/$ //'
  coqc -w -deprecated-native-compiler-option -w -native-compiler-disabled -native-compiler ondemand -R . foo foo.v)

TC: add to :standard

  $ cat > dune <<EOF
  > (coq.theory
  >  (name foo)
  >  (flags :standard -type-in-type))
  > EOF

  $ dune build foo.vo && tail -n 1 _build/log | sed 's/(cd .*coqc/coqc/' | sed 's/$ //'
  coqc -q -type-in-type -w -deprecated-native-compiler-option -w -native-compiler-disabled -native-compiler ondemand -R . foo foo.v)

TC: extend in workspace + override standard

  $ cat > dune <<EOF
  > (coq.theory
  >  (name foo)
  >  (flags :standard))
  > EOF

  $ cat > dune-workspace <<EOF
  > (lang dune 2.8)
  > (env (dev (coq (flags -type-in-type))))
  > EOF

  $ dune build foo.vo && tail -n 1 _build/log | sed 's/(cd .*coqc/coqc/' | sed 's/$ //'
  coqc -type-in-type -w -deprecated-native-compiler-option -w -native-compiler-disabled -native-compiler ondemand -R . foo foo.v)

TC: extend in workspace + override standard

  $ cat > dune-workspace <<EOF
  > (lang dune 2.8)
  > (env (dev (coq (flags :standard -type-in-type))))
  > EOF

  $ dune build foo.vo && tail -n 1 _build/log | sed 's/(cd .*coqc/coqc/' | sed 's/$ //'
  coqc -q -type-in-type -w -deprecated-native-compiler-option -w -native-compiler-disabled -native-compiler ondemand -R . foo foo.v)

TC: extend in dune (env) + override standard

  $ cat > dune <<EOF
  > (coq.theory
  >  (name foo))
  > (env (dev (coq (flags -type-in-type))))
  > EOF

  $ rm -rf _build/default/foo.vo
  $ dune build foo.vo && tail -n 1 _build/log | sed 's/(cd .*coqc/coqc/' | sed 's/$ //'
  coqc -type-in-type -w -deprecated-native-compiler-option -w -native-compiler-disabled -native-compiler ondemand -R . foo foo.v)

TC: extend in dune (env) + standard

  $ cat > dune <<EOF
  > (coq.theory
  >  (name foo))
  > (env (dev (coq (flags :standard -type-in-type))))
  > EOF

  $ rm -rf _build/default/foo.vo
  $ dune build foo.vo && tail -n 1 _build/log | sed 's/(cd .*coqc/coqc/' | sed 's/$ //'
  coqc -q -type-in-type -type-in-type -w -deprecated-native-compiler-option -w -native-compiler-disabled -native-compiler ondemand -R . foo foo.v)

TC: extend in dune (env) + workspace + standard

  $ cat > dune <<EOF
  > (coq.theory
  >  (name foo))
  > (env (dev (coq (flags :standard -bt))))
  > EOF

  $ cat > dune-workspace <<EOF
  > (lang dune 2.8)
  > (env (dev (coq (flags :standard -type-in-type))))
  > EOF

  $ rm -rf _build/default/foo.vo
  $ dune build foo.vo && tail -n 1 _build/log | sed 's/(cd .*coqc/coqc/' | sed 's/$ //'
  coqc -q -type-in-type -bt -w -deprecated-native-compiler-option -w -native-compiler-disabled -native-compiler ondemand -R . foo foo.v)
