.PHONY: all clean test

DEMOS := \
  calc \
  calc-alias \
  calc-ast \
  calc-incremental \
  calc-inspection \
  calc-mini \
  calc-new-syntax \
  calc-param \
  calc-syntax-errors \
  calc-two \
  generate-printers \
  calc-GLR \

ROCQDEMOS := \
  rocq-minicalc \
  rocq-syntax-errors \

all test:
	@ for d in $(DEMOS) ; do echo "Building $$d..." && make -C $$d --no-print-directory $@ ; done
	@ if command -v coqc >/dev/null ; then \
	    if opam list --installed --check coq-menhirlib ; then \
	      for d in $(ROCQDEMOS) ; do echo "Building $$d..." && make -C $$d --no-print-directory $@ ; done ; \
	    else \
	      echo "Skipping the Rocq demos, because coq-menhirlib is not installed." ; \
	    fi \
	  else \
	    echo "Skipping the Rocq demos, because Rocq is not installed." ; \
	  fi

clean:
	git clean -fdX
