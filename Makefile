all: bnfc ccpp

bnfc:
	bnfc CPP.cf
	happy -gca ParCPP.y
	alex -g LexCPP.x

ccpp: ccpp.hs TypeChecker.hs Compiler.hs Env.hs #aca agregar cualquier otro archivo que quieran compilar
	ghc --make ccpp.hs -o ccpp
	#javac Runtime.java
	#cp Runtime.class lab3-testsuite/

clean:
	-rm -f *.log *.aux *.hi *.o *.dvi *~ DocCPP.* LexCPP.* ParCPP.* LayoutCPP.* SkelCPP.* PrintCPP.* TestCPP.* AbsCPP.* TestCPP ErrM.* SharedString.* ComposOp.* CPP.dtd XMLCPP.* ccpp *.class

