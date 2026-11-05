RACO ?= raco
RACKET ?= racket
FILE ?= stlc.rkt

.PHONY: build install repl

build:
	$(RACO) make racket-lang.rkt roulette-lang.rkt \
		racket/lang/reader.rkt roulette/lang/reader.rkt \
		examples/sample.rkt examples/exact.rkt

install:
	$(RACO) pkg install --scope user --skip-installed --link --name roulettecheck .

repl: build
	$(RACKET) -i -e '(enter! "$(FILE)")'
