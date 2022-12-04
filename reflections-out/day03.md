Day 3
===

<!--
This section is generated and compiled by the build script at ./Build.hs from
the file `./reflections/day03.md`.  If you want to edit this, edit
that file instead!
-->

*[all][reflections]* / *[1][day01]* / *[2][day02]* / *3* / *[4][day04]*

[reflections]: https://github.com/mstksg/advent-of-code-2022/blob/main/reflections.md
[day01]: https://github.com/mstksg/advent-of-code-2022/blob/master/reflections-out/day01.md
[day02]: https://github.com/mstksg/advent-of-code-2022/blob/master/reflections-out/day02.md
[day04]: https://github.com/mstksg/advent-of-code-2022/blob/master/reflections-out/day04.md

[Available as an RSS Feed][rss]

[rss]: http://feeds.feedburner.com/jle-advent-of-code-2022

*[Prompt][d03p]* / *[Code][d03g]* / *[Rendered][d03h]*

[d03p]: https://adventofcode.com/2022/day/3
[d03g]: https://github.com/mstksg/advent-of-code-2022/blob/main/src/AOC/Challenge/Day03.hs
[d03h]: https://mstksg.github.io/advent-of-code-2022/src/AOC.Challenge.Day03.html

Some more "string" processing!  First, let's assume that we receive a list of
priority sequences instead of a list of character strings.  Both of these parts
are actually the same problem, the only difference is how we get our groups.
Once we get the groups, we can convert each string into an `IntSet` and find
the mutual intersection between all of the groups with `IS.intersection` and
`foldl1`:

```haskell
import qualified Data.IntSet as IS

solve :: [[Int]] -> Int
solve = sum . map go
  where
    getPriority = IS.findMin . foldl1 IS.intersection . map IS.fromList
```

Then each part is just finding the right splitting function:

```haskell
part1, part2 :: [Int] -> Int
part1 = solve . map (\xs -> chunksOf (length xs `div` 2) xs)
part2 = solve . chunksOf 3
```


*[Back to all reflections for 2022][reflections]*

## Day 3 Benchmarks

```
>> Day 03a
benchmarking...
time                 2.280 ms   (2.143 ms .. 2.436 ms)
                     0.977 R²   (0.958 R² .. 0.995 R²)
mean                 2.239 ms   (2.190 ms .. 2.306 ms)
std dev              186.9 μs   (142.5 μs .. 282.1 μs)
variance introduced by outliers: 59% (severely inflated)

* parsing and formatting times excluded

>> Day 03b
benchmarking...
time                 1.676 ms   (1.586 ms .. 1.780 ms)
                     0.978 R²   (0.967 R² .. 0.988 R²)
mean                 1.565 ms   (1.513 ms .. 1.634 ms)
std dev              191.5 μs   (162.9 μs .. 250.2 μs)
variance introduced by outliers: 78% (severely inflated)

* parsing and formatting times excluded
```

