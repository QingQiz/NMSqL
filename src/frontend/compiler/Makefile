GHC = stack ghc --
GHC_Flags = -c -dynamic -O2

Parser.o: Parser.hs Ast.hs Ast.o
	$(GHC) $(GHC_Flags) $< -o $@


Ast.o: Ast.hs
	$(GHC) $(GHC_Flags) $< -o $@


clean:
	rm -f *.hi *.o



