.PHONY: all
all: objs rts.boot

.PHONY: objs
objs:
	@erl -pa ebin/ -make

rts.boot: rts.rel
	@erl -noshell -pa ebin/ -eval 'systools:make_script("rts", [local]).' -s erlang halt

.PHONY: clean
clean:
	@rm *.boot *.script ebin/*.beam	
