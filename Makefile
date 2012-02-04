SRC := $(wildcard src/*.erl)

all: beam

beam: ${SRC:src/%.erl=ebin/%.beam}

ebin/%.beam: src/%.erl
	erlc -o ebin $<

clean:
	@find ebin -name '*.beam' -print -delete
	@find . -name erl_crash.dump -print -delete

.PHONY: clean
