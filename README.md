# henle

An experimental sentence-mining workflow for language-learning.
The idea originates from the book "Henle's Latin", where you
develop a natural intuition about grammar through repeated drill exercises.


---

## Build & install

```bash
./install.sh  # builds and copies the binary to ~/.local/bin/henle
./install.sh /usr/local/bin # to install somewhere else
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
henle add                     add a new sentence
henle drill [N]               run a drilling session (default: 5 cards)
henle review                  run an SRS review session
henle list [--status STATUS]  list cards (new/drilling/fuzzy/intuitive/mastered)
henle show <id>               show full details for a card
henle edit <id>                edit a card's fields
henle master <id>             mark a card Mastered (suspend from normal rotation)
henle unmaster <id>           return a Mastered card to normal rotation
henle due                     show counts of what's ready to drill/review
```

### A typical day

1. `henle add`: mine 1-5 new sentences day from whatever you read/listened to.
2. `henle drill`: for each new or still-fuzzy sentence, drill it repeatedly
   until it either clicks (mark it intuitive) or doesn't yet (it stays in
   the drill queue for next time).
3. `henle review`: for every card that passed drilling, rate how intuitive it feels
   right now (Easy / Good / Hard). The interval grows on Easy/Good and
   shrinks on Hard, exactly like a normal SRS.
