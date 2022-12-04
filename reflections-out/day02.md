Day 2
===

<!--
This section is generated and compiled by the build script at ./Build.hs from
the file `./reflections/day02.md`.  If you want to edit this, edit
that file instead!
-->

*[all][reflections]* / *[1][day01]* / *2* / *[3][day03]* / *[4][day04]*

[reflections]: https://github.com/mstksg/advent-of-code-2022/blob/master/reflections.md
[day01]: https://github.com/mstksg/advent-of-code-2022/blob/master/reflections-out/day01.md
[day03]: https://github.com/mstksg/advent-of-code-2022/blob/master/reflections-out/day03.md
[day04]: https://github.com/mstksg/advent-of-code-2022/blob/master/reflections-out/day04.md

[Available as an RSS Feed][rss]

[rss]: http://feeds.feedburner.com/jle-advent-of-code-2022

*[Prompt][d02p]* / *[Code][d02g]* / *[Rendered][d02h]*

[d02p]: https://adventofcode.com/2022/day/2
[d02g]: https://github.com/mstksg/advent-of-code-2022/blob/master/src/AOC/Challenge/Day02.hs
[d02h]: https://mstksg.github.io/advent-of-code-2022/src/AOC.Challenge.Day02.html

There's a nice straightforward way to do this by just matching up all 9
combinations, but I had a bit of fun doing it algebraically using `Finite 3`, a
Haskell type that does arithmetic modulo 3, if you assume ABC and XYZ
correspond to 012, respectively.

Basically both parts 1 and 2 involve doing some modular arithmetic to get the
"shape" score, and then some modular arithmetic to get the "outcome" score.

```haskell
type Z3 = Finite 3

play
    :: (Z3 -> Z3 -> Z3)  -- ^ Get shape score
    -> (Z3 -> Z3 -> Z3)  -- ^ Get outcome score
    -> [(Z3, Z3)]
    -> Integer
play shapeScore outcomeScore = sum . map go
  where
    go (x, y) = getFinite (shapeScore x y) + 1
              + getFinite (outcomeScore x y) * 3
```

There is a bit of cute symmetry between `shapeScore` and `outcomeScore` for the
two parts.

```haskell
part1, part2 :: [(Z3, Z3)] -> Integer
part1 = play (\_ y -> y)           (\x y -> y + (1 - x))
part2 = play (\x y -> y - (1 - x)) (\_ y -> y)
```

I mostly just figured it out by using trial and error and taking advantage of
the fact that there are only so many ways you can combine two modulo 3
numbers...but there might be some nice ways to interpret them.

For example, it makes sense that the "shape score" for part 1 is literally just
your shape `y`, and the "outcome score" for part 2 is literally just your
desired outcome `y`

For the outcome score, if you assume that the answer is a "subtraction plus an
offset", then that forces the offset to be 1 in order for a match to represent
a tie.  And so for part 1, the outcome score is found by adding `1-x` to the
shape `y` in order to get the outcome.  So it makes sense that you reverse
the process for part 2: you subtract `1-x` to the outcome `y` in order to get
the shape.  I guess ???


*[Back to all reflections for 2022][reflections]*

## Day 2 Benchmarks

```
>> Day 02a
benchmarking...
time                 346.2 μs   (339.3 μs .. 355.3 μs)
                     0.992 R²   (0.983 R² .. 0.997 R²)
mean                 355.9 μs   (346.8 μs .. 371.2 μs)
std dev              38.09 μs   (21.64 μs .. 62.81 μs)
variance introduced by outliers: 80% (severely inflated)

* parsing and formatting times excluded

>> Day 02b
benchmarking...
time                 529.4 μs   (515.9 μs .. 542.6 μs)
                     0.993 R²   (0.989 R² .. 0.996 R²)
mean                 524.0 μs   (513.0 μs .. 536.6 μs)
std dev              39.80 μs   (34.98 μs .. 47.59 μs)
variance introduced by outliers: 65% (severely inflated)

* parsing and formatting times excluded
```

