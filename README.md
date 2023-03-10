# Crane

## How to Compile

```
sbt clean && sbt compile && sbt assembly
```

## How to Run

```
java -jar target/scala-2.11/forclift-assembly-3.1.jar [-f] [--pdf] [--propinf] [-z] --format-in mln example.mln
```

### Relevant Options
* `-f` to output function definitions
* `--pdf` to visualise the graph before and after smoothing
* `--propinf` to ditch first-order model counting and do propositional model counting instead
* `-z` to (numerically) compute the weighted model count
