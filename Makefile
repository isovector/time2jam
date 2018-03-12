all:
	stack build --fast
	stack exec time2jam
