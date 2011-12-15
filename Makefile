compile:
	rebar compile

test: compile
	erl -noshell -pa ebin -eval "eunit:test([eulogy, eulogy_dir], [verbose])" -s init stop

console: compile
	erl -pa ebin
