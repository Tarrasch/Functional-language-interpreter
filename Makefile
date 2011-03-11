all:
	bnfc grammar.cf
	happy -gca Pargrammar.y
	alex -g Lexgrammar.x
	ghc --make lab4.hs -o lab4

clean:
	-rm -f lab4
	-rm -f *.log *.aux *.hi *.o *.dvi *.o *.tex *grammmar.txt *.hi *.bak
	-rm -f Docgrammar.ps

distclean: clean
	-rm -f Docgrammar.* Lexgrammar.* Pargrammar.* Layoutgrammar.* Skelgrammar.* Printgrammar.* Testgrammar.* Absgrammar.* Testgrammar ErrM.* SharedString.* grammar.dtd XMLgrammar.* 


