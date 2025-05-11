# BurntSushi's torture test
> Taken from https://github.com/BurntSushi/rsc-regexp

The idiomatic Thompson NFA implementation BurntSushi has can execute this in around 0.24s on my
M2 Macbook Pro, with the ripgrep version taking a mere 0.01s. By comparison my runtime looks
to be O(seconds) so there's lots of room for improvement!


### Running the test for ad
To run the test for ad, run the `torture_test` example in release mode
