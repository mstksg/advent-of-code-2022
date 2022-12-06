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
time                 697.4 μs   (677.0 μs .. 723.2 μs)
                     0.990 R²   (0.984 R² .. 0.996 R²)
mean                 750.5 μs   (729.8 μs .. 778.7 μs)
std dev              82.53 μs   (66.84 μs .. 101.5 μs)
variance introduced by outliers: 78% (severely inflated)

* parsing and formatting times excluded

>> Day 06b
benchmarking...
time                 1.161 ms   (1.111 ms .. 1.235 ms)
                     0.984 R²   (0.979 R² .. 0.991 R²)
mean                 1.181 ms   (1.151 ms .. 1.220 ms)
std dev              107.3 μs   (91.78 μs .. 133.7 μs)
variance introduced by outliers: 67% (severely inflated)

* parsing and formatting times excluded
```

