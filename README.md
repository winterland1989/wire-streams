wire-streams 
==============

[![Hackage](https://img.shields.io/hackage/v/wire-streams.svg?style=flat)](http://hackage.haskell.org/package/wire-streams)
[![Build Status](https://travis-ci.org/winterland1989/wire-streams.svg)](https://travis-ci.org/winterland1989/wire-streams)

One stop solution to serialize/deserialize [io-streams](http://hackage.haskell.org/package/io-streams):

+ `System.IO.Streams.Cereal` use [cereal](http://hackage.haskell.org/package/cereal) to serialize/deserialize, it provides sanner default to `Double`(IEEE-754), and `ShortByteString` support.

+  `System.IO.Streams.Binary` use [binary](http://hackage.haskell.org/package/binary) to serialize/deserialize, it provide some useful helpers currently not available in cereal(`getLazyByteStringNul`).

This package is rewritten from [cereal-io-streams](https://github.com/Soostone/cereal-io-streams) and [binary-streams](https://github.com/jonpetterbergman/binary-streams) with following changes:

+ Completely rewrite cereal/io-streams adapter.
+ Clean and unify APIs. 
+ Add more test and benchmark.

Both cereal and binary are top notch serialize/deserialize libaries, you wouldn't go wrong with either choice. This package mainly serve my purpose to develop native mysql adapter, but also provide a benchmark/comparsion across cereal and binary. here's benchmark result against [cereal-conduit](http://hackage.haskell.org/package/cereal-conduit):

```
benchmarking decode one element from cereal-streams/1000 items
time                 135.8 ns   (134.6 ns .. 137.2 ns)
                     0.999 R²   (0.999 R² .. 0.999 R²)
mean                 136.6 ns   (135.2 ns .. 138.2 ns)
std dev              5.147 ns   (4.239 ns .. 6.413 ns)
variance introduced by outliers: 57% (severely inflated)

benchmarking decode one element cereal-conduit/1000 items
time                 340.1 ns   (337.0 ns .. 343.6 ns)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 339.9 ns   (335.6 ns .. 344.6 ns)
std dev              13.68 ns   (10.59 ns .. 18.03 ns)
variance introduced by outliers: 58% (severely inflated)

benchmarking decode 1000 elements from cereal-streams/1000 items
time                 112.5 μs   (111.6 μs .. 113.4 μs)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 112.8 μs   (111.4 μs .. 114.4 μs)
std dev              4.735 μs   (3.522 μs .. 6.811 μs)
variance introduced by outliers: 43% (moderately inflated)

benchmarking decode 1000 elements cereal-conduit/1000 items
time                 204.4 μs   (201.9 μs .. 206.9 μs)
                     0.995 R²   (0.990 R² .. 0.997 R²)
mean                 220.7 μs   (209.5 μs .. 271.0 μs)
std dev              66.92 μs   (9.094 μs .. 152.6 μs)
variance introduced by outliers: 98% (severely inflated)
```


