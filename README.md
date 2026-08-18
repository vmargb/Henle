# henle

An experimental sentence-mining workflow for language-learning.

---

## What is it?

The idea originates from the book "Henle's Latin".

You *collect* difficult sentences, or sentences that you will **actually use**,
then practise drilling them over and over until you hit an *"aha"* moment, meaning
it made sense without any effort and **without thinking of the english**. The
sentence now moves over from drilling mode into the SRS queue, where a review session
tests for whether the *"aha"* is still fresh in your mind or should return back to drilling.

However many repetition you take at any point influences the overall algorithm.


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
`./install.sh /usr/local/bin` to install somewhere else

or for Windows:
```ps
.\install.ps1
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
