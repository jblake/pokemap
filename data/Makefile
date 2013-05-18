GENERATED := $(basename $(wildcard *.m4))
TILESETS := $(addsuffix .tiles.bin, $(basename $(basename $(wildcard *.tiles.m4))) $(basename $(wildcard *.tiles)))

default: ${GENERATED} ${TILESETS}

clean:
	rm -f ${GENERATED}
	rm -f ${TILESETS}

.PHONY: clean default

%: %.m4
	m4 $< > $@

%.bin: %
	xxd -r -p $< > $@
