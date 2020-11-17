stone:
	dune build

install: stone
	dune install

doc: stone
	dune exec src/stone.exe -- doc/

clean:
	dune clean

mrproper: clean
	rm -rf doc/site/

.PHONY: stone install doc clean mrproper
