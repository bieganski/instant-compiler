
all:	ParInstant.hs
	alex -g LexInstant.x
	ghc --make TestInstant.hs -o TestInstant
	rm -rf TestInstant.hs

jvm:
	ghc  --make insc_jvm.hs -o insc_jvm

debug: info.txt

ParInstant.y LexInstant.x: Instant.cf
	bnfc $<

ParInstant.hs: ParInstant.y
	happy -gca $<

info.txt: ParInstant.y
	happy -gca ParInstant.y
clean:
	-rm -f *.log *.aux *.hi *.o *.dvi *.*~
	-rm -f DocInstant.ps
distclean: clean
	-rm -f DocInstant.* LexInstant.* ParInstant.* LayoutInstant.* SkelInstant.* PrintInstant.* TestInstant.* AbsInstant.* TestInstant ErrM.* SharedString.* Instant.dtd XMLInstant.* info.txt

