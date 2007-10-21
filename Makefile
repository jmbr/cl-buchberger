.PHONY: all clean dist

PACKAGE := cl-buchberger

all:
	a2ps -o groebner.ps *.lisp
	ps2pdf groebner.ps
	rm groebner.ps

clean:
	rm -f groebner.pdf *.fasl
	rm -f *~

dist:
	mkdir -p dist
	VERSION=`shtool version VERSION`; \
	shtool fixperm *; \
	shtool tarball --output dist/$(PACKAGE)-$$VERSION.tar.gz \
			--compress 'gzip -9' --user jmbr --group jmbr \
			--directory cl-buchberger-$$VERSION `darcs query manifest`
