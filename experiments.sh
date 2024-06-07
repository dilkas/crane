#!/usr/bin/env sh

for f in ../practical-fomc/data/MLNs/sequences/a*.mln; do
    echo;
    echo "$f";
    echo;
    GREEDY=false timeout 5 java -jar target/scala-2.11/crane-assembly-1.0.jar -n --format-in mln "$f";
done
