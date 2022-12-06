Day 6
===

<!--
This section is generated and compiled by the build script at ./Build.hs from
the file `./reflections/day06.md`.  If you want to edit this, edit
that file instead!
-->

*[all][reflections]* / *[1][day01]* / *[2][day02]* / *[3][day03]* / *[4][day04]* / *6*

[reflections]: https://github.com/mstksg/advent-of-code-2022/blob/main/reflections.md
[day01]: https://github.com/mstksg/advent-of-code-2022/blob/master/reflections-out/day01.md
[day02]: https://github.com/mstksg/advent-of-code-2022/blob/master/reflections-out/day02.md
[day03]: https://github.com/mstksg/advent-of-code-2022/blob/master/reflections-out/day03.md
[day04]: https://github.com/mstksg/advent-of-code-2022/blob/master/reflections-out/day04.md

[Available as an RSS Feed][rss]

[rss]: http://feeds.feedburner.com/jle-advent-of-code-2022

*[Prompt][d06p]* / *[Code][d06g]* / *[Rendered][d06h]*

[d06p]: https://adventofcode.com/2022/day/6
[d06g]: https://github.com/mstksg/advent-of-code-2022/blob/main/src/AOC/Challenge/Day06.hs
[d06h]: https://mstksg.github.io/advent-of-code-2022/src/AOC.Challenge.Day06.html

This is a good opportunity to use the ol' sliding windows trick in Haskell list
processing!

Basically if you want sliding windows over a list `"abcdefg"` to get, ie,
`["abc","bcd","cde","def","efg"]`, one thing you can do is iteratively drop items:

```haskell
map (`drop` "abcdefg")  [0,1,2]
-- ["abcdefg","bcdefg","cdefg","defg"]
```

and then take the transpose:

```
transpose $ map (`drop` "abcdefg")  [0,1,2]
-- ["abc","bcd","cde","def","efg","fg","g"]
```

You get the sliding windows, plus a few trailing elements as the window slides
off the edge of the list.

Now at this point we just need to find the first unique sliding window:

```haskell
windows :: Int -> [a] -> [[a]]
windows n xs = transpose $ map (`drop` xs) [0..n-1]

solve :: Int -> String -> Int
solve n xs = find isUnique ws + n
  where
    ws = windows n xs
    isUnique w = S.size (S.fromList w) == n


part1, part2 :: String -> Int
part1 = solve 4
part2 = solve 14
```


*[Back to all reflections for 2022][reflections]*

## Day 6 Benchmarks

```
>> Day 06a
benchmarking...
time                 1.105 ms   (1.049 ms .. 1.175 ms)
                     0.979 R²   (0.967 R² .. 0.991 R²)
mean                 1.142 ms   (1.103 ms .. 1.177 ms)
std dev              130.6 μs   (114.9 μs .. 155.8 μs)
variance introduced by outliers: 77% (severely inflated)

* parsing and formatting times excluded

>> Day 06b
benchmarking...
time                 1.684 ms   (1.584 ms .. 1.779 ms)
                     0.973 R²   (0.960 R² .. 0.987 R²)
mean                 1.803 ms   (1.730 ms .. 1.870 ms)
std dev              264.0 μs   (226.6 μs .. 293.8 μs)
variance introduced by outliers: 84% (severely inflated)

* parsing and formatting times excluded
```

