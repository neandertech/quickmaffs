all: repl lsp interpreter

clean:
	rm -rf bin/*
	scala-cli clean .

bin/repl:
	mkdir -p bin
	scala-cli package . -M REPL -f -o bin/repl

bin/lsp:
	mkdir -p bin
	scala-cli package . -M LSP -f -o bin/lsp

bin/quickmaffs:
	mkdir -p bin
	scala-cli package . -M INTERPRETER -f -o bin/quickmaffs

repl: bin/repl 
lsp: bin/lsp 
interpreter: bin/quickmaffs 

