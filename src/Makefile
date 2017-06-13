all:
	happy -gca ParMokka.y
	alex -g LexMokka.x
	ghc --make Main.hs -o Main

clean:
	-rm -f *.log *.aux *.hi *.o *.dvi

distclean: clean
	-rm -f DocMokka.* LexMokka.* ParMokka.* LayoutMokka.* PrintMokka.* AbsMokka.* ErrM.* SharedString.* ComposOp.* Mokka.dtd XMLMokka.* Makefile*
