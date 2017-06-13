all:
	#happy -gca ParMokka.y
	#alex -g LexMokka.x
	+$(MAKE) -C src
	mv ./src/Interpreter ./Interpreter

clean:
	-rm -f *.log *.aux *.hi *.o *.dvi
	-rm Interpreter
	cd ./src && $(MAKE) clean
