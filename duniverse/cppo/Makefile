.PHONY: all clean test check install uninstall release

all:
	@dune build

clean:
	@git clean -fX

test:
	@dune runtest

check: test

install:
	@dune install

uninstall:
	@dune uninstall

# To make a release:
# + check that [make test] succeeds
# + check that everything has been committed and pushed
# + check that the CI has succeeded
# + make sure that the package is not pinned: [opam pin remove cppo]
# + run [make release VERSION=X.Y.Z]

release:
# Check if this is the master branch.
	@ if [ "$$(git symbolic-ref --short HEAD)" != "master" ] ; then \
	  echo "Error: this is not the master branch." ; \
	  git branch ; \
	  exit 1 ; \
	fi
# Check if everything has been committed.
	@ if [ -n "$$(git status --porcelain)" ] ; then \
	    echo "Error: there remain uncommitted changes." ; \
	    git status ; \
	    exit 1 ; \
	  fi
# Make sure the current version can be compiled.
	@ make clean
	@ make test
# Check the current package description.
	@ opam lint
# Make sure $(VERSION) is nonempty.
	@ if [ -z "$(VERSION)" ] ; then \
	    echo "Error: please use: make release VERSION=X.Y.Z" ; \
	    exit 1 ; \
	  fi
# Make sure a CHANGES entry with the current version seems to exist.
	@ if ! grep "## v$(VERSION)" Changes.md >/dev/null ; then \
	    echo "Error: Changes.md has no entry for version $(VERSION)." ; \
	    exit 1 ; \
	  fi
# Make sure the current version is mentioned in dune-project.
	@ if ! grep "(version $(VERSION))" dune-project >/dev/null ; then \
	    echo "Error: dune-project does not mention version $(VERSION)." ; \
	    grep "(version" dune-project ; \
	    exit 1 ; \
	  fi
# Create a git tag.
	@ git tag v$(VERSION)
# Upload. (This automatically makes a .tar.gz archive available on github.)
	@ git push
	@ git push --tags
# Publish an opam description.
	@ opam publish --tag=v$(VERSION) -v $(VERSION) ocaml-community/cppo
