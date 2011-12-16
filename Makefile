compile:
	rebar compile

test: compile
	erl -noshell -pa ebin -eval "application:start(crypto), application:start(emysql), eunit:test([eulogy, eulogy_dir], [verbose])" -s init stop

console: compile
	erl -pa ebin -eval "application:start(crypto), application:start(emysql), application:start(eulogy)."
