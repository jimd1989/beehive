# beehive

Serve up answers to the New York Times [Spelling Bee](https://www.nytimes.com/puzzles/spelling-bee) puzzle.

The game rules are as follows:

- You have a set of seven letters.
- How many real words (not proper nouns) can you make from them?
- Every word must contain the first letter in the set.
- Letters can be used multiple times.
- Every word created is worth one point.
- If a word contains all seven letters in the set, it is worth three points.

## Building

```
stack build
stack install
```

If the compiled binary isn't placed where you want it, just `cp` it to `/usr/local/bin` or wherever.

## Usage

Spin the service up with

```
beehive <port number> <dictionary file>
```

This will start a warp server that listens to GET requests on `<port number>`, and provides answers using `<dictionary file>`, where `<dictionary file>` is a newline-delimited plaintext file of words, like `/usr/share/dict/words`.

Any request to `host:port/answer/abcdefg`, where `abcdefg` is the lowercase seven letter set, and `a` is the essential letter, will return all valid words from the dictionary file. Three point values will be written at the top of the response, separated by a blank line.

This program was written as a web server with the intention of other endpoints being used to generate puzzles, but it's rather barebones for the time being. Error handling is not verbose either. The core feature—fetching answers—is extremely fast though.
