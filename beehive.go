package main

import (
	"bufio"
	"fmt"
	"log"
	"net/http"
	"os"
	"runtime"
	"strings"
	"sync"
	"time"
)

type Entry struct {
	Hash uint32
	Text string
}

type Search struct {
	Compliment uint32
	Main uint32
	All uint32
	Response *log.Logger
}

var nThreads = runtime.NumCPU()
var dict = makeDictionary()

func complement(s string) uint32 {
	var h uint32
	az := [26]uint32{}
	for _, c := range s {
		az[uint32(c) % 26] = 1
	}
	for i := 0 ; i < 26 ; i++ {
		h |= az[i] << uint32(i)
	}
	return ^h
}

func hash(s string) uint32 {
	var h uint32
	for _, c := range s {
		h |= 1 << (uint32(c) % 26)
	}
	return h
}

func makeSearch(s string, wr http.ResponseWriter) Search {
	return Search{
		complement(s),
		1 << (uint32(s[0]) % 26),
		hash(s),
		log.New(wr, "", 0),
	      }
}

func check(se *Search, e *Entry) {
	if e.Hash & se.Main == 0 {
		return
	}
	if e.Hash & se.Compliment != 0 {
		return
	}
	if e.Hash == se.All {
		se.Response.Println(strings.ToUpper(e.Text))
	} else {
		se.Response.Println(e.Text)
	}
}

func checkAll(se *Search, n int, w *sync.WaitGroup) {
	for ; n < len(dict) ; n += nThreads {
		check(se, &dict[n])
	}
	w.Done()
}

func makeDictionary() []Entry {
	var d []Entry
	if len(os.Args) != 3 {
		panic("usage: beehive <dict file> <port>")
	}
	fi, err := os.Open(os.Args[1])
	if err != nil {
		panic("beehive: error opening file")
	}
	defer fi.Close()
	s := bufio.NewScanner(fi)
	fmt.Fprintf(os.Stderr, "Creating dictionary from %s\n", os.Args[1])
	for s.Scan() {
		t := s.Text()
		d = append(d, Entry{hash(t), t})
	}
	fmt.Fprintf(os.Stderr, "Dictionary loaded\n")
	return d
}

func searchDictionary(s string, wr http.ResponseWriter) {
	start := time.Now()
	var w sync.WaitGroup
	se := makeSearch(s, wr)
	for i := 0 ; i < nThreads ; i++ {
		w.Add(1)
		go checkAll(&se, i, &w)
	}
	w.Wait()
	fmt.Fprintf(os.Stderr, "served in %v\n", time.Since(start))
}

func handler(wr http.ResponseWriter, r *http.Request) {
	keys, ok := r.URL.Query()["search"]
	if !ok || len(keys) < 1 {
		fmt.Fprintf(wr, "Use https://url/?search=abcdefg\n")
		fmt.Fprintf(os.Stderr,
			    "beehive: %v Empty request\n", time.Now())
		return
	}
	if len(keys[0]) != 7 {
		fmt.Fprintf(wr, "Queries must be exactly 7 lowercase chars.\n")
		fmt.Fprintf(os.Stderr, 
			    "beehive: %v Malformed request\n", time.Now())
		return
	}
	fmt.Fprintf(os.Stderr,
	            "beehive: %v Request for \"%s\" ", time.Now(), keys[0])
	searchDictionary(keys[0], wr)
}

func main () {
	fmt.Printf("Listening on port %s\n", os.Args[2])
	http.HandleFunc("/", handler)
	err := http.ListenAndServe(":" + os.Args[2], nil)
	if err != nil {
		panic("Problem listening. Possible malformed port?")
	}
}
