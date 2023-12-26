#!/usr/bin/env sh

for f in data/sequences/*.mln; do
    echo;
    echo "$f";
    echo;
    timeout 5 java -jar target/scala-2.11/crane-assembly-1.0.jar -n --format-in mln "$f";
done
