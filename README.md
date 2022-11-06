# Функциональное программирование
## Лабораторная №1 


### Вариант 11, 20

<b>Выполнил:</b> Лыщев Иван Антонович \
<b>Группа:</b> P34112 \
<b>Преподаватель:</b> Пенской Александр Владимирович

### Задача 11
<b>Условие</b> \
Найти наибольшее произведение 4х подряд идущих чисел. Как в ряд, так и по диагонали
<img src="./pics/Euler11.png">

### Описание решения 
Написание основной логики я начал с создания гибкого итератора по ячейкам, чтобы его можно было переиспользовать для всех случаев. Решил использовать замыкания, чтобы можно было просто вызывать функцию next() для получения следующего значения
```f#
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
```
Затем написал “перемножитель” для ячейки. Т.е. он перемножает N чисел, начиная с ячейки (x,y), в заданном направлении
```f#
let getMult (data:int[,]) nextFunc (x:int) (y:int)  : int =
    let mutable mult : int = 1;
    let next = nextFunc data x y 
    for i = 0 to n-1 do
        let v = next()
        mult <- mult * v
    mult
```
Далее создал функцию, которая проходится по срезу и считает для каждой ячейки произведение. После находится максимум в матрице.
```f#
let getMax (data:int[,]) (x_0, y_0) (x_n, y_n)  wayFunc =
    let indexes = Array2D.init (x_n-x_0) (y_n-y_0) (fun x y -> (x+x_0,y+y_0))
    let resArr = indexes |> Array2D.map (fun (x,y) -> getMult data wayFunc x y)
    let maxV = [for i in 0 .. (resArr.GetLength(0) - 1) -> resArr.[i, *]] |> List.map (fun v -> Array.max v) |> List.max
    maxV
```
Здесь создается список значение от 0 до 3] и в зависимости от значения, выбирается “путь” перемножения.
```f#
[for i in 0..3 -> getMaxPerWay data i] |> List.max |> printfn "%d"
```
Для реализации выбора пути была написана следующая функция
```f#
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
```
### Ответ
Ответом на заданную матрицу было получено число 70.600.674

### Задача 11
<b>Условие</b> \
Найти сумму цифр в 100!

### Описание решения 
Сама функция факториала была получена с помощью свертки списка чисел от 1 до n. 
```f#
let multiply x y = x * y
    let factorial n : bigint = List.fold multiply 1I [1I .. n]
```
Затем число bigint преобразуется в строку, каждый символ переводится в число и они суммируются.
```f#
let value = factorial 100I |> sprintf "%A" |> Seq.map (fun v -> int( string( v))) |> Seq.fold (fun sum v -> sum + v) 0
    printfn $"{value}"
```
### Альтернативное решение
В цикле добавлять к sum последнюю цифру и делить основное число на 10.
```f#
    let mutable fact = factorial 100I
    let mutable sum = 0I
    while fact > 0I do
        sum <- sum + fact%10I
        fact <- fact / 10I
```

### Рекурсивное решение
```f#
let rec symbSum (x: bigint) : bigint =
        let mutable sum = x % 10I
        if x > 0I then
            sum <- sum + symbSum (x/10I)
        sum
    let sum100 = symbSum (factorial 100I)
```

### Ответ
648

### Выводы
В ходе написания данной лабораторной работы я был приятно удивлен F#. До этого я не имел дела с функциональными ЯП, но при этом трудностей у меня тоже не возникло. Многие выражения получаются более изящными чем в ООП и при этом занимают меньше строк. Новым было использовать функции как данные, но это позволяет легко их переиспользовать для разных целей. Пока не всегда получается обходится только immutable переменными, поскольку они требуют другого подхода к коду. 