#!/usr/bin/env ruby

files = `ls -1 ebin/*.beam`.split.map {|f| /([a-z_]*).beam/.match(f); $1 }

puts `erl -noshell -pa ebin/ -eval 'eunit:test([#{files.join(',')}]).' -s erlang halt`
