object = ../../../bin/shunting_yard

$(object): main.cc expression.cc function_call.cc token.cc
	g++ -w -g -Wall -std=c++17 $^ -o $@ -lgmpxx -lgmp

clean:
	rm $(object)
