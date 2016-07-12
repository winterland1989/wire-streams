wire-streams 
==============

[![Hackage](https://img.shields.io/hackage/v/wire-streams.svg?style=flat)](http://hackage.haskell.org/package/wire-streams)
[![Build Status](https://travis-ci.org/winterland1989/wire-streams.svg)](https://travis-ci.org/winterland1989/wire-streams)

One stop solution to serialize/deserialize [io-streams](http://hackage.haskell.org/package/io-streams):

+ `System.IO.Streams.Cereal` use [cereal](http://hackage.haskell.org/package/cereal) to serialize/deserialize, cereal provides sanner default to `Double`(IEEE-754), and `ShortByteString` support.

+  `System.IO.Streams.Binary` use [binary](http://hackage.haskell.org/package/binary) to serialize/deserialize, binary provide some useful helpers currently not available in cereal(`getLazyByteStringNul`).

This package is rewritten from [cereal-io-streams](https://github.com/Soostone/cereal-io-streams) and [binary-streams](https://github.com/jonpetterbergman/binary-streams) with following changes:

+ Completely rewrite cereal/io-streams adapter.
+ Clean and unify APIs. 
+ Add more test and benchmark.

Both cereal and binary are top notch serialize/deserialize libaries, you wouldn't go wrong with either choice. This package mainly serve my purpose to develop native mysql adapter, but also provide a benchmark/comparsion across cereal and binary. here's benchmark result against [cereal-conduit](http://hackage.haskell.org/package/cereal-conduit):

```
benchmarking decode one element wire-streams/cereal/1000 items
time                 126.7 ns   (125.1 ns .. 128.2 ns)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 127.4 ns   (126.1 ns .. 128.9 ns)
std dev              4.887 ns   (4.122 ns .. 6.214 ns)
variance introduced by outliers: 58% (severely inflated)

benchmarking decode one element wire-streams/binary/1000 items
time                 218.4 ns   (216.8 ns .. 220.0 ns)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 217.5 ns   (215.8 ns .. 219.2 ns)
std dev              5.588 ns   (4.589 ns .. 7.044 ns)
variance introduced by outliers: 37% (moderately inflated)

benchmarking decode one element cereal-conduit/1000 items
time                 318.5 ns   (314.7 ns .. 322.1 ns)
                     0.999 R²   (0.999 R² .. 0.999 R²)
mean                 319.2 ns   (316.1 ns .. 322.7 ns)
std dev              11.37 ns   (8.824 ns .. 15.09 ns)
variance introduced by outliers: 53% (severely inflated)

benchmarking decode 1000 elements from wire-streams/cereal/1000 items
time                 99.61 μs   (98.56 μs .. 100.9 μs)
                     0.997 R²   (0.994 R² .. 0.999 R²)
mean                 100.4 μs   (98.83 μs .. 102.5 μs)
std dev              6.321 μs   (4.136 μs .. 9.830 μs)
variance introduced by outliers: 64% (severely inflated)

benchmarking decode 1000 elements from wire-streams/binary/1000 items
time                 189.3 μs   (187.0 μs .. 191.5 μs)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 189.3 μs   (187.5 μs .. 190.9 μs)
std dev              5.868 μs   (4.966 μs .. 7.174 μs)
variance introduced by outliers: 27% (moderately inflated)

benchmarking decode 1000 elements cereal-conduit/1000 items
time                 203.3 μs   (201.1 μs .. 205.7 μs)
                     0.998 R²   (0.996 R² .. 0.999 R²)
mean                 204.0 μs   (201.3 μs .. 207.9 μs)
std dev              10.38 μs   (7.759 μs .. 14.73 μs)
variance introduced by outliers: 49% (moderately inflated)
```
