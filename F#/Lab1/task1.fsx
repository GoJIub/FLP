let a = 0.
let b = 1.
let segments = 10
let eps = 1e-10

let iter f i a b =
    let rec iter' a acc =
        match a > b with
        | true -> acc
        | false -> iter' (a + 1) (f a acc)

    iter' a i

let power x =
    iter (fun _ acc -> acc * x) 1. 1

let builtin x =
    1. / (4. - power x 4)

let rec sumUntil getTerm nextState state sum terms =
    let term = getTerm state

    let continueOrStop = function
        | true -> sumUntil getTerm nextState (nextState state) (sum + term) (terms + 1)
        | false -> sum, terms

    continueOrStop (abs term >= eps)

let dumbTerm x n =
    power x (4 * n) / power 4. (n + 1)

let dumbTaylor x =
    sumUntil (dumbTerm x) (fun n -> n + 1) 0 0. 0

let smartTaylor x =
    let ratio = power x 4 / 4.
    sumUntil id (fun term -> term * ratio) 0.25 0. 0

let printHeader =
    printfn "f(x) = 1 / (4 - x^4), x in [%.1f; %.1f], eps = %.1e" a b eps
    printfn "%6s | %14s | %14s | %7s | %14s | %7s" "x" "Builtin" "Smart Taylor" "# terms" "Dumb Taylor" "# terms"
    printfn "-----------------------------------------------------------------------------------------------"

let rec printRows i =
    let proceed = function
        | true ->
            let x = a + float i * (b - a) / float segments
            let fx = builtin x
            let smartValue, smartTerms = smartTaylor x
            let dumbValue, dumbTerms = dumbTaylor x

            printfn "%6.3f | %14.10f | %14.10f | %7d | %14.10f | %7d"
                x
                fx
                smartValue
                smartTerms
                dumbValue
                dumbTerms

            printRows (i + 1)
        | false -> ()

    proceed (i <= segments)

let printTable =
    printHeader
    printRows 0

printTable
