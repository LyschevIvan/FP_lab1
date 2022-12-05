module Tests

open Xunit
open Lab1

[<Theory>]
[<InlineData("Euler11_input.txt", 70600674)>]
[<InlineData("Euler11_test_1.txt", 16)>]
[<InlineData("Euler11_test_2.txt", 120)>]
[<InlineData("Euler11_test_3.txt", 120)>]
[<InlineData("Euler11_test_4.txt", 120)>]
[<InlineData("Euler11_test_5.txt", 120)>]
let ``Source data tail rec test`` (path, exp) =
    let data = initData path
    let maxValueTailRec = getMaxTailRec data
    Assert.Equal(exp, maxValueTailRec)

[<Theory>]
[<InlineData("Euler11_input.txt", 70600674)>]
[<InlineData("Euler11_test_1.txt", 16)>]
[<InlineData("Euler11_test_2.txt", 120)>]
[<InlineData("Euler11_test_3.txt", 120)>]
[<InlineData("Euler11_test_4.txt", 120)>]
[<InlineData("Euler11_test_5.txt", 120)>]
let ``Source data rec test`` (path, exp) =
    let data = initData path
    let maxValueRec = getMaxRec data
    Assert.Equal(exp, maxValueRec)

[<Theory>]
[<InlineData("Euler11_input.txt", 70600674)>]
[<InlineData("Euler11_test_1.txt", 16)>]
[<InlineData("Euler11_test_2.txt", 120)>]
[<InlineData("Euler11_test_3.txt", 120)>]
[<InlineData("Euler11_test_4.txt", 120)>]
[<InlineData("Euler11_test_5.txt", 120)>]
let ``Source data module test`` (path, exp) =
    let data = initData path
    let maxValueRec = getMaxRec data
    Assert.Equal(exp, maxValueRec)

[<Theory>]
[<InlineData(10, 3628800)>]
[<InlineData(2, 2)>]
let ``Test fact value`` (n: bigint, value) =
    let v = factorial n
    Assert.Equal(value, v)

[<Theory>]
[<InlineData(12345, 15)>]
[<InlineData(101010201, 6)>]
let ``Test rec symbol sum`` (number, value) =
    let sum = symbSum number
    Assert.Equal(value, sum)

[<Theory>]
[<InlineData(12345, 15)>]
[<InlineData(101010201, 6)>]
let ``Test tail rec symbol sum`` (number, value) =
    let sum = getSymbSumTail number
    Assert.Equal(value, sum)

[<Theory>]
[<InlineData(12345, 15)>]
[<InlineData(101010201, 6)>]
let ``Test module symbol sum`` (number, value) =
    let sum = moduleSymbSum number
    Assert.Equal(value, sum)

[<Theory>]
[<InlineData(100, 648)>]
[<InlineData(10, 27)>]
let ``Test factorial digit sum module`` (n, value) =
    let sum = moduleFact n
    Assert.Equal(value, sum)


[<Theory>]
[<InlineData(100, 648)>]
[<InlineData(10, 27)>]
let ``Test factorial digit sum rec`` (n, value) =
    let sum = recFact n
    Assert.Equal(value, sum)

[<Theory>]
[<InlineData(100, 648)>]
[<InlineData(10, 27)>]
let ``Test factorial digit sum tail rec`` (n, value) =
    let sum = tailRecFact n
    Assert.Equal(value, sum)
