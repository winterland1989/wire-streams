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
time                 143.8 ns   (141.8 ns .. 146.2 ns)
                     0.998 R²   (0.998 R² .. 0.999 R²)
mean                 143.8 ns   (142.4 ns .. 145.9 ns)
std dev              5.936 ns   (4.520 ns .. 8.427 ns)
variance introduced by outliers: 61% (severely inflated)

benchmarking decode one element wire-streams/binary/1000 items
time                 287.8 ns   (285.1 ns .. 290.4 ns)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 286.7 ns   (284.5 ns .. 289.5 ns)
std dev              8.172 ns   (6.484 ns .. 11.74 ns)
variance introduced by outliers: 42% (moderately inflated)

benchmarking decode one element cereal-conduit/1000 items
time                 371.8 ns   (360.9 ns .. 389.0 ns)
                     0.994 R²   (0.989 R² .. 0.999 R²)
mean                 364.5 ns   (361.2 ns .. 371.5 ns)
std dev              16.21 ns   (9.194 ns .. 28.64 ns)
variance introduced by outliers: 63% (severely inflated)

benchmarking decode 1000 elements from wire-streams/cereal/1000 items
time                 114.3 μs   (113.2 μs .. 115.5 μs)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 116.8 μs   (115.4 μs .. 119.4 μs)
std dev              6.483 μs   (4.390 μs .. 10.86 μs)
variance introduced by outliers: 56% (severely inflated)

benchmarking decode 1000 elements from wire-streams/binary/1000 items
time                 269.3 μs   (266.0 μs .. 273.5 μs)
                     0.994 R²   (0.986 R² .. 0.998 R²)
mean                 276.8 μs   (271.4 μs .. 288.6 μs)
std dev              25.12 μs   (14.29 μs .. 42.50 μs)
variance introduced by outliers: 75% (severely inflated)

benchmarking decode 1000 elements cereal-conduit/1000 items
time                 221.2 μs   (218.1 μs .. 224.4 μs)
                     0.997 R²   (0.994 R² .. 0.998 R²)
mean                 227.0 μs   (223.5 μs .. 234.6 μs)
std dev              17.09 μs   (9.635 μs .. 26.98 μs)
variance introduced by outliers: 68% (severely inflated)
```
