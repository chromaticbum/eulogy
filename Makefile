compile:
	rebar compile

test: compile
	erl -noshell -pa ebin -eval "eunit:test([eulogy, eulogy_dir, eulogy_migration], [verbose])" -s init stop

console: compile
	erl -pa ebin -eval "application:start(eulogy)."

mysql_console: compile
	erl -pa ../eulogy/ebin -pa ../eu_mysql_adapter/ebin -eval "application:start(crypto), application:start(emysql), application:start(eulogy), application:start(eu_mysql_adapter)."

