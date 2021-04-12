all:
	dune build && cp -f _build/default/main.exe eff-c && chmod 755 eff-c

clean:
	dune clean
	rm -rf _build eff-c
