Day 4
===

<!--
This section is generated and compiled by the build script at ./Build.hs from
the file `./reflections/day04.md`.  If you want to edit this, edit
that file instead!
-->

*[all][reflections]* / *[1][day01]* / *[2][day02]* / *[3][day03]* / *4* / *[6][day06]*

[reflections]: https://github.com/mstksg/advent-of-code-2022/blob/main/reflections.md
[day01]: https://github.com/mstksg/advent-of-code-2022/blob/master/reflections-out/day01.md
[day02]: https://github.com/mstksg/advent-of-code-2022/blob/master/reflections-out/day02.md
[day03]: https://github.com/mstksg/advent-of-code-2022/blob/master/reflections-out/day03.md
[day06]: https://github.com/mstksg/advent-of-code-2022/blob/master/reflections-out/day06.md

[Available as an RSS Feed][rss]

[rss]: http://feeds.feedburner.com/jle-advent-of-code-2022

*[Prompt][d04p]* / *[Code][d04g]* / *[Rendered][d04h]*

[d04p]: https://adventofcode.com/2022/day/4
[d04g]: https://github.com/mstksg/advent-of-code-2022/blob/main/src/AOC/Challenge/Day04.hs
[d04h]: https://mstksg.github.io/advent-of-code-2022/src/AOC.Challenge.Day04.html

A bit of fun number crunching :)  Here is a chance to leverage an interval
library, like haskell's
*[data-interval](https://hackage.haskell.org/package/data-interval)*:

```haskell
import           Data.IntegerInterval (IntegerInterval)
import qualified Data.IntegerInterval as I

part1Criterion :: IntegerInterval -> IntegerInterval -> Bool
part1Criterion xs ys = xs `I.isSubsetOf` ys || ys `I.isSubsetOf` xs

part2Criterion :: IntegerInterval -> IntegerInterval -> Bool
part2Criterion = (I.==?)
```

From there on it's just a matter of running the criteria on each pair of
intervals in the list and counting which ones are valid!


*[Back to all reflections for 2022][reflections]*

## Day 4 Benchmarks

```
>> Day 04a
benchmarking...
time                 23.44 μs   (23.10 μs .. 23.82 μs)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 23.24 μs   (23.16 μs .. 23.43 μs)
std dev              404.9 ns   (219.1 ns .. 761.3 ns)
variance introduced by outliers: 14% (moderately inflated)

* parsing and formatting times excluded

>> Day 04b
benchmarking...
time                 56.35 μs   (54.69 μs .. 57.86 μs)
                     0.994 R²   (0.991 R² .. 0.998 R²)
mean                 55.18 μs   (54.13 μs .. 56.75 μs)
std dev              4.232 μs   (3.144 μs .. 6.017 μs)
variance introduced by outliers: 74% (severely inflated)

* parsing and formatting times excluded
```

