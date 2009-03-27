all: MGrammar.dll

MGrammar.dll : MGrammar.cs
	smcs -debug -r:Irony.dll -t:library MGrammar.cs

MGrammar.cs : MGrammar.txt MGrammarGenerator.exe
	mono --debug MGrammarGenerator.exe MGrammar.txt MGrammar.cs

MGrammarGenerator.exe : MGrammarGenerator.cs
	gmcs -debug MGrammarGenerator.cs

dist:
	mkdir -p mmm
	cp Irony.dll MGrammarGenerator.cs MGrammar.txt README Makefile mmm
	rm mmm.tar.bz2
	tar jcf mmm.tar.bz2 mmm
