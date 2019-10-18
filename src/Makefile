all:
	bnfc Instant.cf
	happy -gca ParInstant.y
	alex -g LexInstant.x
	ghc --make TestInstant.hs -o TestInstant

clean:
	rm -f *.log *.aux *.hi *.o *.dvi
