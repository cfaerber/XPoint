
PASDOC=pasdoc --staronly --write-uses-list -M

doc: doc-html
	
doc-html: allunits.html
	
allunits.html: ../*.pas Makefile
	$(PASDOC) -E . ..\*.pas
