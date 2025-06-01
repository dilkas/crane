# Crane 2
**Crane 2** is a *first-order model counter*, built on top of [ForcLift](https://github.com/UCLA-StarAI/Forclift).

## ⚠️ Project Status
This is a **prototype**. While it supports a range of inference tasks, additional engineering effort is required for robustness and broader applicability.

## 🔧 Building Crane 2
To build the project, make sure the dependencies below are installed, then run:

```
make
```

To clean and rebuild from scratch:

```
make clean && make
```

### Build Requirements
* **Java 8**
* **Scala 2.11**
* For graphical visualisation:
  * `dot2tex`
  * `Evince`
  * `Graphviz`
  * `pdfTeX`

## 🚀 Running Crane 2
```
[SKOLEMIZE=true] [GREEDY=false] [DEPTH=n] [SOLUTIONS=n] \
java -jar target/scala-2.11/crane-assembly-1.0.jar \
[-n] [-d <n>] [-e <n>] [--pdf] [-t <timeout>] [-v <verbosity>] example.mln
```

### Main Arguments
- `example.mln`: Input file specifying a Markov Logic Network. Follows syntax similar to [Alchemy](https://alchemy.cs.washington.edu/user-manual/manual.html).
- To compute model counts (instead of just performing compilation), use one of the following options:
  - `-n`: Run inference using domain sizes defined in `example.mln`.
  - `-d <n>`: Run inference on domain sizes 2<sup>0</sup>, 2<sup>1</sup>, ..., 2<sup>n</sup>, using the same value for all domain sizes. If n = 0, the process runs indefinitely (or until timeout).
  - `-e <n>`: Same as `-d <n>`, but uses domain sizes 1, 2, ..., n.
- `--pdf`: Visualise the graph before and after smoothing.
- `-t <timeout>`: Set a time limit (in seconds) for both compilation and inference (separately, default: no limit).
- `-v <verbosity>`: Set verbosity level (default: 0; accepted values: 0, 1, and 2).

### Optional Modes
- `GREEDY=false`: Enable hybrid search mode instead of greedy compilation. Additional search parameters:
  - `DEPTH=n`: Limit the depth of the search tree (default: no limit).
  - `SOLUTIONS=n`: Stop after finding `n` solutions (default: 1).
- `SKOLEMIZE=true`: Only perform Skolemization and unit propagation (use together with `-v 2`). This option is useful for converting instances to other formats with no support for existential quantifiers.

## 📄 Related Publications
- **Kidambi A. K., Singh G., Dilkas P., Meel K. S.** (2025). *Towards Practical First-Order Model Counting*. In *Proceedings of SAT 2025*.
- **Dilkas P., Belle V.** (2023). *Synthesising Recursive Functions for First-Order Model Counting: Challenges, Progress, and Conjectures*. In *Proceedings of KR 2023*.
- **Dilkas P.** *Generalising Weighted Model Counting*. University of Edinburgh 2023.

## 👥 Contributors
- [**Paulius Dilkas**](https://dilkas.github.io/) — University of Toronto, Canada  
- **Ananth K. Kidambi** — Indian Institute of Technology Bombay, India  
- **Guramrit Singh** — Indian Institute of Technology Bombay, India
