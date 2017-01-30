# Big Ear

Fetches price data from Kraken (a cryptocurrency exchange).

## Installation

1) Clone the repo in a directory that makes it "visible" for `quicklisp` such as `~/common-lisp` or `~/quicklisp/local-projects`.

2) [Optional] Configure the fetching interval.

3) In the REPL:

```
(ql:quickload :big-ear)
(big-ear:start)
```
