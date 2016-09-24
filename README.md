wire-streams 
==============

[![Hackage](https://img.shields.io/hackage/v/wire-streams.svg?style=flat)](http://hackage.haskell.org/package/wire-streams)
[![Build Status](https://travis-ci.org/winterland1989/wire-streams.svg)](https://travis-ci.org/winterland1989/wire-streams)

Serialize/deserialize `ByteString` streams from [io-streams](http://hackage.haskell.org/package/io-streams) using [binary](http://hackage.haskell.org/package/binary) package.

From 0.1 cereal is removed because [a binary performance problem](https://github.com/winterland1989/binary-parsers/blob/master/Data/Binary/Parser.hs#L187) is taken care of by [binary-parsers](https://github.com/winterland1989/binary-parsers) package. Now binary is much faster and featureful.

Benchmark
---------

```
benchmarking decode one element wire-streams/cereal/1000 items
time                 127.6 ns   (126.4 ns .. 129.2 ns)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 128.1 ns   (126.6 ns .. 130.1 ns)
std dev              6.019 ns   (4.441 ns .. 9.245 ns)
variance introduced by outliers: 67% (severely inflated)

benchmarking decode one element wire-streams/binary/1000 items
time                 90.49 ns   (89.77 ns .. 91.11 ns)
                     0.999 R²   (0.999 R² .. 0.999 R²)
mean                 90.42 ns   (89.44 ns .. 91.41 ns)
std dev              3.371 ns   (2.711 ns .. 4.433 ns)
variance introduced by outliers: 57% (severely inflated)

benchmarking decode one element cereal-conduit/1000 items
time                 345.2 ns   (325.0 ns .. 382.3 ns)
                     0.953 R²   (0.896 R² .. 0.999 R²)
mean                 338.6 ns   (329.5 ns .. 379.4 ns)
std dev              52.15 ns   (14.85 ns .. 114.3 ns)
variance introduced by outliers: 95% (severely inflated)

benchmarking decode 1000 elements from wire-streams/cereal/1000 items
time                 97.24 μs   (95.95 μs .. 98.57 μs)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 97.75 μs   (96.78 μs .. 98.68 μs)
std dev              3.170 μs   (2.640 μs .. 3.906 μs)
variance introduced by outliers: 31% (moderately inflated)

benchmarking decode 1000 elements from wire-streams/binary/1000 items
time                 65.24 μs   (64.69 μs .. 65.94 μs)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 66.09 μs   (65.35 μs .. 67.62 μs)
std dev              3.601 μs   (2.014 μs .. 6.391 μs)
variance introduced by outliers: 58% (severely inflated)

benchmarking decode 1000 elements cereal-conduit/1000 items
time                 198.4 μs   (195.4 μs .. 201.3 μs)
                     0.998 R²   (0.997 R² .. 0.999 R²)
mean                 200.4 μs   (197.9 μs .. 203.7 μs)
std dev              9.718 μs   (7.707 μs .. 12.77 μs)
variance introduced by outliers: 47% (moderately inflated)
```
