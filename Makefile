# Copyright © 2008 Bart Massey
# ALL RIGHTS RESERVED
# [This program is licensed under the "3-clause ('new') BSD License"]
# Please see the file COPYING in the source
# distribution of this software for license terms.

HC = ghc
HCFLAGS = -O -Wall

.SUFFIXES: .hs
.hs.o:
	$(HC) $(HCFLAGS) --make $*.hs

StratifyTests: Stratify.o
	$(HC) $(HCFLAGS) --make StratifyTests.hs

clean:
	-rm -f *.hi *.o StratifyTests stratify.tar.gz

sdist: clean
	mkdir stratify
	cp COPYING *.hs Makefile stratify
#	cp proposal.txt stratify/README
	tar czf stratify.tar.gz stratify
	rm -rf stratify
