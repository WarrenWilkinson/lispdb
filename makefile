SBCL := /usr/bin/sbcl

all: formlis.core 
formlis.core:
	$(SBCL) --load "src/load.lisp" --eval "(make-image)"

deploy: src/css/style.gz
	gzip -c src/css/print.css > src/css/print.gz
	tar -C ../ -cvjf /tmp/deploy.tar.bz2 version3.2

src/css/style.gz: src/css/style.css
	gzip -c src/css/style.css > src/css/style.gz



.PHONY: formlis.core deploy



