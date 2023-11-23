all: scala cpp

scala:
	sbt compile assembly

cpp:
	cd src/main/cpp && make

clean:
	sbt clean
	cd src/main/cpp && make clean
