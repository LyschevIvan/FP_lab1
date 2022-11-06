module lab1
open System
let n = 4
let a = 20
let b = 20

let initData : int[,] =
    let data = array2D [[08; 2; 22; 97; 38; 15; 0; 40; 0; 75; 4; 5; 7; 78; 52; 12; 50; 77; 91; 8];
                [49; 49; 99; 40; 17; 81; 18; 57; 60; 87; 17; 40; 98; 43; 69; 48; 4; 56; 62; 0];
                [81; 49; 31; 73; 55; 79; 14; 29; 93; 71; 40; 67; 53; 88; 30; 3; 49; 13; 36; 65];
                [52; 70; 95; 23; 4; 60; 11; 42; 69; 24; 68; 56; 1; 32; 56; 71; 37; 2; 36; 91];
                [22; 31; 16; 71; 51; 67; 63; 89; 41; 92; 36; 54; 22; 40; 40; 28; 66; 33; 13; 80];
                [24; 47; 32; 60; 99; 3; 45; 2; 44; 75; 33; 53; 78; 36; 84; 20; 35; 17; 12; 50];
                [32; 98; 81; 28; 64; 23; 67; 10; 26; 38; 40; 67; 59; 54; 70; 66; 18; 38; 64; 70];
                [67; 26; 20; 68; 2; 62; 12; 20; 95; 63; 94; 39; 63; 8; 40; 91; 66; 49; 94; 21];
                [24; 55; 58; 5; 66; 73; 99; 26; 97; 17; 78; 78; 96; 83; 14; 88; 34; 89; 63; 72];
                [21; 36; 23; 9; 75; 0; 76; 44; 20; 45; 35; 14; 0; 61; 33; 97; 34; 31; 33; 95];
                [78; 17; 53; 28; 22; 75; 31; 67; 15; 94; 3; 80; 4; 62; 16; 14; 9; 53; 56; 92];
                [16; 39; 5; 42; 96; 35; 31; 47; 55; 58; 88; 24; 0; 17; 54; 24; 36; 29; 85; 57];
                [86; 56; 0; 48; 35; 71; 89; 7; 5; 44; 44; 37; 44; 60; 21; 58; 51; 54; 17; 58];
                [19; 80; 81; 68; 5; 94; 47; 69; 28; 73; 92; 13; 86; 52; 17; 77; 4; 89; 55; 40];
                [4; 52; 8; 83; 97; 35; 99; 16; 7; 97; 57; 32; 16; 26; 26; 79; 33; 27; 98; 66];
                [88; 36; 68; 87; 57; 62; 20; 72; 3; 46; 33; 67; 46; 55; 12; 32; 63; 93; 53; 69];
                [4; 42; 16; 73; 38; 25; 39; 11; 24; 94; 72; 18; 8; 46; 29; 32; 40; 62; 76; 36];
                [20; 69; 36; 41; 72; 30; 23; 88; 34; 62; 99; 69; 82; 67; 59; 85; 74; 4; 36; 16];
                [20; 73; 35; 29; 78; 31; 90; 1; 74; 31; 49; 71; 48; 86; 81; 16; 23; 57; 5; 54];
                [1; 70; 54; 71; 83; 51; 54; 69; 16; 92; 33; 48; 61; 43; 52; 1; 89; 19; 67; 48]]
    data

let way inc_x inc_y = 
    let next (data:int[,]) (x:int) (y:int)  =
        let mutable incX : int = 0
        let mutable incY : int = 0
        let func() : int  = 
            let v = data.[y+incY, x+incX]
            incX <- (incX + inc_x)
            incY <- (incY + inc_y)
            v
        func
    next

let getMult (data:int[,]) nextFunc (x:int) (y:int)  : int =
    let mutable mult : int = 1;
    let next = nextFunc data x y 
    for i = 0 to n-1 do
        let v = next()
        mult <- mult * v
    mult

let getMax (data:int[,]) (x_0, y_0) (x_n, y_n)  wayFunc =
    let indexes = Array2D.init (x_n-x_0) (y_n-y_0) (fun x y -> (x+x_0,y+y_0))
    let resArr = indexes |> Array2D.map (fun (x,y) -> getMult data wayFunc x y)
    let maxV = [for i in 0 .. (resArr.GetLength(0) - 1) -> resArr.[i, *]] |> List.map (fun v -> Array.max v) |> List.max
    maxV
            
let getMaxPerWay data wayNum =
    let maxFunc = getMax data 
    let mutable ret = 0
    if wayNum = 0 then
        ret <- maxFunc (0, 0) (a, b-n+1) (way 0 1)
    elif wayNum = 1 then
        ret <- maxFunc (0, 0) (a-n+1, b) (way 1 0)
    elif wayNum = 2 then
        ret <- maxFunc (0, 0) (a-n+1, b-n+1) (way 1 1)
    else
        ret <- maxFunc (n-1, 0) (a, b-n+1) (way -1 1)
    ret

[<EntryPoint>]
let main argv =

    printfn "Project Euler 11"
    // let rnd = new Random()
    //let data = Array2D.init a b (fun x y -> rnd.Next(1,100)) // for random 2D array
    let data = initData
    // printfn "%A" data
    [for i in 0..3 -> getMaxPerWay data i] |> List.max |> printfn "%d"
  
    printfn "Project Euler 20"
    let multiply x y = x * y
    let factorial n : bigint = List.fold multiply 1I [1I .. n]
    let value = factorial 100I |> sprintf "%A" |> Seq.map (fun v -> int( string( v))) |> Seq.fold (fun sum v -> sum + v) 0
    printfn $"{value}"
    
    let mutable fact = factorial 100I
    let mutable sum = 0I
    while fact > 0I do
        sum <- sum + fact%10I
        fact <- fact / 10I
    printfn $"{sum}"
    
    let rec symbSum (x: bigint) : bigint =
        let mutable sum = x % 10I
        if x > 0I then
            sum <- sum + symbSum (x/10I)
        sum
    let sum100 = symbSum (factorial 100I)
    printfn $"{sum100}"
    0