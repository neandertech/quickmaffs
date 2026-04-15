all: repl lsp interpreter

clean:
	rm -rf bin/*
	scala-cli clean .

setup:
	scala-cli setup-ide *.scala


bin/repl: *.scala
	mkdir -p bin
	scala-cli package . --main-class REPL --force --output bin/repl

bin/lsp: *.scala
	mkdir -p bin
	scala-cli package . --main-class LSP --force --output bin/lsp 

bin/lsp-native: *.scala
	mkdir -p bin
	scala-cli package . --main-class LSP --force --output bin/lsp-native --native


bin/quickmaffs: *.scala
	mkdir -p bin
	scala-cli package . --main-class INTERPRETER --force --output bin/quickmaffs

repl: bin/repl
lsp: bin/lsp
interpreter: bin/quickmaffs

