MGRAMMAR_DLL_SOURCES = MGrammar.cs MGrammar.Generated.cs
M2CS_SOURCES = driver.cs

all: m2cs.exe

m2cs.exe : MGrammar.dll $(M2CS_SOURCES)
	gmcs -debug -r:Irony.dll -r:MGrammar.dll $(M2CS_SOURCES) -out:m2cs.exe

MGrammar.dll : $(MGRAMMAR_DLL_SOURCES)
	gmcs -debug -r:Irony.dll -t:library $(MGRAMMAR_DLL_SOURCES) -out:MGrammar.dll

MGrammar.Generated.cs : MGrammar.txt MGrammarGenerator.exe
	mono --debug MGrammarGenerator.exe MGrammar.txt MGrammar.Generated.cs

MGrammarGenerator.exe : MGrammarGenerator.cs
	gmcs -debug MGrammarGenerator.cs

clean:
	rm MGrammar.dll MGrammarGenerator.exe MGrammar.Generated.cs

dist:
	mkdir -p mmm
	cp Irony.dll MGrammarGenerator.cs MGrammar.txt README Makefile mmm
	rm mmm.tar.bz2
	tar jcf mmm.tar.bz2 mmm
