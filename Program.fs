module Lab1

open System
open Microsoft.FSharp.Collections

let N = 4
let a = 20
let b = 20

let initData path : int[,] =
    let f (text: string) =
        let arr =
            text.Split([| '\n' |], StringSplitOptions.RemoveEmptyEntries)
            |> Array.map (fun l -> l.Split([| ' ' |], StringSplitOptions.RemoveEmptyEntries))
            |> array2D
            |> Array2D.map (int)

        arr

    let data = IO.File.ReadAllText(__SOURCE_DIRECTORY__ + @"/" + path) |> f
    data


let way x =
    if x = 0 then (0, 1)
    elif x = 1 then (1, 0)
    elif x = 2 then (1, 1)
    else (-1, 1)

let getMult (data: int[,]) way x y =
    let incX, incY = way

    let rec innerMult mult xI yI n =
        if n <= 0 then
            mult
        else
            innerMult (mult * data[yI, xI]) (xI + incX) (yI + incY) (n - 1)

    innerMult 1 x y N

let getMax (data: int[,]) (x0, y0) (xN, yN) wayFunc =
    let indexes = Array2D.init (xN - x0) (yN - y0) (fun x y -> (x + x0, y + y0))
    let resArr = indexes |> Array2D.map (fun (x, y) -> getMult data wayFunc x y)

    let maxV =
        [ for i in 0 .. (resArr.GetLength(0) - 1) -> resArr.[i, *] ]
        |> List.map (Array.max)
        |> List.max

    maxV

let getMaxPerWay data wayNum =
    let maxFunc = getMax data

    if wayNum = 0 then
        maxFunc (0, 0) (a, b - N + 1) (way wayNum)
    elif wayNum = 1 then
        maxFunc (0, 0) (a - N + 1, b) (way wayNum)
    elif wayNum = 2 then
        maxFunc (0, 0) (a - N + 1, b - N + 1) (way wayNum)
    else
        maxFunc (N - 1, 0) (a, b - N + 1) (way wayNum)

let getMaxRec data =
    let getMultiplied (data: int[,]) way (x, y) n =
        let incX, incY = way

        if
            (a - (x + n * incX) < 0 || b - (y + n * incY) < 0)
            || (x + n * incX < 0 || y + n * incY < 0)
        then
            0
        else
            let rec innerMultiplier xI yI nI =
                if nI <= 0 then
                    1
                else
                    data[yI, xI] * innerMultiplier (xI + incX) (yI + incY) (nI - 1)

            let res = innerMultiplier x y n
            res

    let getMaxForWay data way =
        let rec iterY data yI =
            let rec iterX data way yI xI =
                if xI >= a - 1 then
                    0
                else
                    let mult = getMultiplied data way (xI, yI) N
                    let max = iterX data way yI (xI + 1)
                    if (mult > max) then mult else max

            if yI >= b - 1 then
                0
            else
                let thisRowMax = iterX data way yI 0
                let rowMax = iterY data (yI + 1)
                if rowMax > thisRowMax then rowMax else thisRowMax

        iterY data 0

    let rec findMaxThroughWays n =
        if n < 0 then
            0
        else
            let thisMax = getMaxForWay data (way n)
            let max = findMaxThroughWays (n - 1)
            if max > thisMax then max else thisMax

    findMaxThroughWays 3

let getMaxTailRec data =
    let getMultiplied (data: int[,]) way (x, y) n =
        let incX, incY = way

        if
            (a - (x + n * incX) < 0 || b - (y + n * incY) < 0)
            || (x + n * incX < 0 || y + n * incY < 0)
        then
            0
        else
            let rec innerMultiplier res xI yI nI =
                if nI <= 0 then
                    res
                else
                    innerMultiplier (data[yI, xI] * res) (xI + incX) (yI + incY) (nI - 1)

            let res = innerMultiplier 1 x y n
            res

    let getMaxForWay data way =
        let rec iterY data max yI =
            let rec iterX data way max yI xI =
                if xI >= a - 1 then
                    max
                else
                    let mult = getMultiplied data way (xI, yI) N

                    if (mult > max) then
                        iterX data way mult yI (xI + 1)
                    else
                        iterX data way max yI (xI + 1)

            if yI >= b - 1 then
                max
            else
                let thisRowMax = iterX data way 0 yI 0

                if thisRowMax > max then
                    iterY data thisRowMax (yI + 1)
                else
                    iterY data max (yI + 1)

        iterY data 0 0

    let rec findMaxThroughWays n max =
        if n < 0 then
            max
        else
            let thisMax = getMaxForWay data (way n)

            if thisMax > max then
                findMaxThroughWays (n - 1) thisMax
            else
                findMaxThroughWays (n - 1) max

    findMaxThroughWays 3 0

[<EntryPoint>]
let main argv : int =

    printfn "Project Euler 11"
    //let data = Array2D.init a b (fun x y -> rnd.Next(1,100)) // for random 2D array
    let data = initData "Euler11_input.txt"

    let maxValueRec = getMaxRec data
    let maxValueTailRec = getMaxTailRec data

    let maxValueModule =
        [| 0..3 |]
        |> Array.map (getMaxPerWay data)
        |> Array.fold (fun max i -> if max > i then max else i) 0

    printfn $"Max value module impl. = {maxValueModule}"
    printfn $"Max value recursion impl. = {maxValueRec}"
    printfn $"Max value tail recursion impl = {maxValueTailRec}"




    printfn "Project Euler 20"
    let multiply x y = x * y
    let factorial n : bigint = List.fold multiply 1I [ 1I .. n ]

    let valueModule = factorial 100I |> sprintf "%A" |> Seq.sumBy (string >> int)

    printfn $"Max value module impl. = {valueModule}"

    let rec symbSum (x: bigint) : bigint =
        let sum = x % 10I

        if x > 0I then
            sum + (symbSum (x / 10I)) // обычная рекурсия
        else
            sum

    let valueRec = symbSum (factorial 100I)
    printfn $"Max value recursion impl. = {valueRec}"

    let getSymbSumTail (x: bigint) =
        let rec symbSumTailInner (x: bigint) sum : bigint =
            if x > 0I then
                symbSumTailInner (x / 10I) (sum + x % 10I) // хвостовая рекурсия
            else
                sum

        symbSumTailInner x 0I

    let valueRecTail = getSymbSumTail (factorial 100I)
    printfn $"Max value tail recursion impl = {valueRecTail}"
    0
