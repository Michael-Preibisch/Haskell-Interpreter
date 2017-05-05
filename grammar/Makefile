all:
	happy -gca ParMokka.y
	alex -g LexMokka.x
	ghc --make TestMokka.hs -o TestMokka

clean:
	-rm -f *.log *.aux *.hi *.o *.dvi

distclean: clean
	-rm -f DocMokka.* LexMokka.* ParMokka.* LayoutMokka.* SkelMokka.* PrintMokka.* TestMokka.* AbsMokka.* TestMokka ErrM.* SharedString.* ComposOp.* mokka.dtd XMLMokka.* Makefile*
	

