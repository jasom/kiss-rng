This is an implementation of George Marsaglia's super-long-period KISS PRNG.

It has a period on the order of 2 to the power of 1.3 million; it's speed in
SBCL is comparable to the built-in random-chunk function, at least on the
machines I've tested.  There are both 32 and 64 bit versions.


https://groups.google.com/forum/#!msg/sci.math/go58i7GH2Ck/0pd7cqUciV0J
