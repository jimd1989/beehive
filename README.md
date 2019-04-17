A web application version of [beehive](https://github.com/jamesiandalrymple/beehive): a program that can be used to solve or generate New York Times spelling bee puzzles like the following:

[![puzzle with main letter U and secondary letters T A E I N P](https://dalrym.pl/media/img/beehive-puzzle.png)](https://dalrym.pl/media/img/beehive-puzzle.png)

## Building

    go build beehive.go

## Usage

The C version of the program operates once upon a stream from stdin, but this version loads its set of words ahead of time and accepts HTTP queries. Run beehive with:

    beehive <dictionary file> <port number>

where the dictionary is a plaintext file of words delimited by newlines (such as /usr/share/dict/web2 on BSD systems) and the port number is whatever port beehive should listen on. A pre-filtered dictionary file (no proper nouns; only words 5 chars or longer) has been included, but it's sourced from a public domain work that is considerably outdated.

Searches can be made with a simple GET request:

    http://localhost:<beehive port>/?search=xxxxxxx

where xxxxxxx is 7 lowercase ASCII chars that will be used in the puzzle. The first char is the most significant one; all words returned will contain it. Three point words will be written in CAPS.

## Demo

You can see the program in action at the bottom of [this entry](https://dalrym.pl/projects/beehive.html) on my personal website.

## Issues

+ beehive will not complain if uppercase or unicode glyphs are provided. It will respond to all queries 7 bytes in length. Of course the output will not be what one expects.
+ The returned list of words is not guaranteed to be alphabetized. This is not essential to the puzzle.
