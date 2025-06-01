object = ../../../bin/shunting_yard

$(object): main.cc expression.cc function_call.cc token.cc
	g++ -g -Wall $^ -o $@ -lgmpxx -lgmp

clean:
	-rm $(object)
