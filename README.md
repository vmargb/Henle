# henle

An experimental sentence-mining workflow for language-learning.
The idea originating from the book "Henle's Latin", where you
develop a natural intuition for grammar through repeated drill exercises.


---

## Build from source

To build it from source, you need **OCaml** and **Dune** installed on your system.

**Linux (Debian/Ubuntu)**:
```bash
sudo apt-get install ocaml dune
```

**MacOS**:
```bash
brew install ocaml dune
```

**Windows**:
```bash
winget install Git.Git OCaml.opam
```

then just run (Linux & MacOS):
```bash
git clone https://github.com/vmargb/Henle.git
cd Henle
./install.sh  # builds and copies the binary to ~/.local/bin/henle
```
or `./install.sh /usr/local/bin` to install somewhere else

or for Windows:
# Builds and copies the binary to ~\.local\bin\henle.exe
```ps
.\install.ps1
```

**Requirements**:
```bash
# Debian/Ubuntu
sudo apt-get install ocaml-nox ocaml-dune
```

Or just build without installing:

```bash
dune build
./_build/default/bin/main.exe
```

## Usage

```
henle add [--lang LANG]                     add a new sentence
henle drill [N] [--lang LANG]               run a drilling session (default: 5 cards)
henle review [--lang LANG]                  run an SRS review session
henle list [--status STATUS] [--lang LANG]  list cards (new/drilling/fuzzy/intuitive/mastered)
henle show <id>                             show full details for a card
henle edit <id>                             edit a card's fields
henle master <id>                           mark a card Mastered (suspend from normal rotation)
henle unmaster <id>                         return a Mastered card to normal rotation
henle due [--lang LANG]                     show counts of what's ready to drill/review
henle languages                             list languages in the deck, with card counts
```

### A typical day

1. `henle add`: mine 1-5 new sentences day from whatever you read/listened to.
2. `henle drill`: for each new or still-fuzzy sentence, drill it repeatedly
   until it either clicks (mark it intuitive) or doesn't yet (it stays in
   the drill queue for next time).
3. `henle review`: for every card that passed drilling, rate how intuitive it feels
   right now (Easy / Good / Hard). The interval grows on Easy/Good and
   shrinks on Hard, exactly like a normal SRS.
