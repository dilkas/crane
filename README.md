# Crane

## How to Compile

```
sbt clean && sbt compile && sbt assembly
```

## How to Run

```
[GREEDY=false] [PARALLEL=true] [DEPTH=n] [SOLUTIONS=n] java -jar target/scala-2.11/forclift-assembly-3.1.jar [-f] [--pdf] [--propinf] [-z] --format-in mln example.mln
```

### Relevant Options
* `-f` to output function definitions
* `--pdf` to visualise the graph before and after smoothing
* `GREEDY=false` to run in hybrid search mode. Additional options:
  * `DEPTH=n` to stop the search once the search tree reaches depth `n + 1`
  * `SOLUTIONS=n` to stop the search once we have `n` solutions
* `-z` to (numerically) compute the weighted model count. Additional options:
  * `--propinf` to ditch first-order model counting and do propositional model counting instead
  * `PARALLEL=true` to evaluate each solution on its own thread (assuming that `GREEDY=false`)
