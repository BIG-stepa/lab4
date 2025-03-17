open System
open System.Globalization

// Функция для проверки, является ли введенное значение вещественным числом (float)
let rec checkFloat (prompt: string) =
    printf "%s" prompt
    let input = Console.ReadLine()
    match Double.TryParse(input, NumberStyles.Any, CultureInfo.InvariantCulture) with
    | true, number ->
        Some number
    | false, _ ->
        if input = "stop" then None // Если введено "stop", завершаем ввод
        else
            printfn "Введите вещественное или целое число, или 'stop' для завершения."
            checkFloat prompt

// Определение структуры бинарного дерева
type BinaryTree =
    | Node of float * BinaryTree * BinaryTree
    | Empty

// Функция map для бинарного дерева
let rec map (f: float -> float) (tree: BinaryTree) =
    match tree with
    | Node (value, left, right) -> Node (f value, map f left, map f right)
    | Empty -> Empty

// Функция fold для обхода бинарного дерева
let rec fold (f: 'a -> BinaryTree -> 'a) (acc: 'a) (tree: BinaryTree) =
    match tree with
    | Empty -> acc
    | Node (value, left, right) ->
        let accLeft = fold f acc left
        let accCurrent = f accLeft tree
        fold f accCurrent right

// Функция для сбора листьев с использованием fold
let collectLeaves (tree: BinaryTree) =
    fold (fun acc currentTree ->
        match currentTree with
        | Node (value, Empty, Empty) -> value :: acc // Лист, если нет потомков
        | _ -> acc
    ) [] tree |> List.rev

// Функция для вставки значения в дерево
let rec insert tree value =
    match tree with
    | Empty -> Node (value, Empty, Empty)
    | Node (v, left, right) ->
        if value < v then
            Node (v, insert left value, right)
        elif value > v then
            Node (v, left, insert right value)
        else
            // Если значение уже существует в дереве, не добавляем его снова
            tree

// Функция для ввода дерева
let rec inputTree () =
    printfn "Введите значение узла (или 'stop' для завершения ввода):"
    match checkFloat "" with
    | Some value ->
        insert (inputTree ()) value
    | None ->
        Empty // Если введено "stop", возвращаем пустое дерево

// Функция для отбрасывания дробной части
let truncateFloat (x: float) = int x |> float

// Функция для генерации строки из n пробелов
let spaces n = String.replicate n " "

// Функция для красивого вывода дерева с учетом направления
let rec printTree (tree: BinaryTree) (level: int) (direction: string) =
    match tree with
    | Empty -> ()
    | Node (value, left, right) ->
        // Выводим правое поддерево с увеличенным уровнем
        printTree right (level + 1) "right"
        
        // Вычисляем отступы с учетом направления
        let indent =
            if direction = "root" then
                "" // Нет отступов для корня
            elif direction = "right" then
                spaces (level * 3) // Отступы для правых узлов
            else
                spaces (level * 2) // Меньшие отступы для левых узлов
        
        // Выводим текущий узел с отступами
        printfn "%s%.2f" indent value
        
        // Выводим левое поддерево с увеличенным уровнем
        printTree left (level + 1) "left"

// Функция для вызова красивого вывода дерева с начальным уровнем 0
let printPrettyTree (tree: BinaryTree) =
    printTree tree 0 "root"

// Задача 1: Отбрасывание дробной части с использованием map
let z1 () =
    printfn "Введите значения узлов дерева (вещественные числа). Введите 'stop', чтобы завершить ввод."
    let tree = inputTree ()

    // Красивый вывод исходного дерева
    printfn "\nИсходное дерево:"
    printPrettyTree tree

    // Используем map для отбрасывания дробной части
    let truncatedTree = map truncateFloat tree

    // Красивый вывод измененного дерева
    printfn "\nДерево с отброшенной дробной частью:"
    printPrettyTree truncatedTree

// Задача 2: Сбор листьев с использованием fold
let z2 () =
    printfn "Введите значения узлов дерева (вещественные числа). Введите 'stop', чтобы завершить ввод."
    let tree = inputTree ()

    // Красивый вывод исходного дерева
    printfn "\nИсходное дерево:"
    printPrettyTree tree

    // Собираем листья с использованием fold
    let leaves = collectLeaves tree
    printfn "\nСписок узлов-листьев: %A" leaves

// Основная функция
let main () =
    let rec loop () =
        printfn "Введите номер программы для проверки: "
        printfn "1 - Tree.map (отбрасывание дробной части)"
        printfn "2 - Tree.fold (сбор листьев)"
        printfn "0 - Выход из программы"
        printf "Введите номер задачи: "
        match checkFloat "" with
        | Some input ->
            match int input with
            | 1 -> z1 ()
            | 2 -> z2 ()
            | 0 -> printfn "Выход из программы..."
            | _ -> printfn "Неверный ввод! Пожалуйста, выберите число от 0 до 2."
            if int input <> 0 then loop ()
        | None ->
            printfn "Неверный ввод! Пожалуйста, введите число."
            loop ()
    loop ()

// Запуск программы
main ()