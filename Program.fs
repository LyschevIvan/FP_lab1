module Program

open Lab1

[<EntryPoint>]
let main _ : int =
    printfn "Project Euler 11"
    let data = initData "Euler11_input.txt"

    let maxValueRec = getMaxRec data
    let maxValueTailRec = getMaxTailRec data
    let maxValueModule = getMaxValueModule data

    printfn $"Max value module impl. = {maxValueModule}"
    printfn $"Max value recursion impl. = {maxValueRec}"
    printfn $"Max value tail recursion impl = {maxValueTailRec}"

    printfn "Project Euler 20"

    printfn $"Max value module impl. = {moduleFact 100I}"
    printfn $"Max value recursion impl. = {recFact 100I}"
    printfn $"Max value tail recursion impl = {tailRecFact 100I}"
    0
