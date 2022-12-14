all: repl lsp interpreter

clean:
	rm -rf bin/*
	scala-cli clean .

setup:
	scala-cli setup-ide *.scala


bin/repl:
	mkdir -p bin
	scala-cli package . --main-class REPL --force --output bin/repl

bin/lsp:
	mkdir -p bin
	scala-cli package . --main-class LSP --force --output bin/lsp 

bin/quickmaffs:
	mkdir -p bin
	scala-cli package . --main-class INTERPRETER --force --output bin/quickmaffs

repl: bin/repl
lsp: bin/lsp
interpreter: bin/quickmaffs

