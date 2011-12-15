compile:
	rebar compile

test: compile
	erl -noshell -pa ebin -eval "eunit:test(eulogy_dir_test, [verbose])" -s init stop
