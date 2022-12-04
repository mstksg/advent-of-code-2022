Day 1
===

<!--
This section is generated and compiled by the build script at ./Build.hs from
the file `./reflections/day01.md`.  If you want to edit this, edit
that file instead!
-->

*[all][reflections]* / *1* / *[2][day02]* / *[3][day03]* / *[4][day04]*

[reflections]: https://github.com/mstksg/advent-of-code-2022/blob/master/reflections.md
[day02]: https://github.com/mstksg/advent-of-code-2022/blob/master/reflections-out/day02.md
[day03]: https://github.com/mstksg/advent-of-code-2022/blob/master/reflections-out/day03.md
[day04]: https://github.com/mstksg/advent-of-code-2022/blob/master/reflections-out/day04.md

[Available as an RSS Feed][rss]

[rss]: http://feeds.feedburner.com/jle-advent-of-code-2022

*[Prompt][d01p]* / *[Code][d01g]* / *[Rendered][d01h]*

[d01p]: https://adventofcode.com/2022/day/1
[d01g]: https://github.com/mstksg/advent-of-code-2022/blob/master/src/AOC/Challenge/Day01.hs
[d01h]: https://mstksg.github.io/advent-of-code-2022/src/AOC.Challenge.Day01.html

Day 1's are usually pretty good for Haskell's stream processing, and this is no
exception :)

To get the list of elf calories, we split on the double newlines, and take the
sum of the lines of each group.

```haskell
import Data.List.Split (splitOn)

getCalories :: String -> [Int]
getCalories = map (sum . map read . lines) . splitOn "\n\n"
```

For part 1, this involves just finding the maximum.

```haskell
part1 :: String -> Int
part1 = maximum . getCalories
```

For part 2, we find the sum of the top 3 values:

```haskell
part2 :: String -> Int
part2 = sum . take 3 . reverse . sort . getCalories
```


*[Back to all reflections for 2022][reflections]*

## Day 1 Benchmarks

```
>> Day 01a
benchmarking...
time                 118.2 μs   (114.1 μs .. 123.5 μs)
                     0.984 R²   (0.973 R² .. 0.990 R²)
mean                 114.8 μs   (110.9 μs .. 122.2 μs)
std dev              18.66 μs   (10.65 μs .. 35.57 μs)
variance introduced by outliers: 93% (severely inflated)

* parsing and formatting times excluded

>> Day 01b
benchmarking...
time                 230.3 μs   (220.7 μs .. 243.6 μs)
                     0.975 R²   (0.966 R² .. 0.986 R²)
mean                 263.8 μs   (251.5 μs .. 279.3 μs)
std dev              47.28 μs   (36.51 μs .. 56.97 μs)
variance introduced by outliers: 92% (severely inflated)

* parsing and formatting times excluded
```

