all:
	sbt compile assembly
	cd src/main/cpp && make

clean:
	sbt clean
	cd src/main/cpp && make clean
