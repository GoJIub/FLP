let a = 0.0
let b = 1.0
let segments = 10
let eps = 1e-10

let builtin x =
    1.0 / (4.0 - pown x 4)

let rec sumUntil getTerm nextState state sum terms =
    let term = getTerm state

    if abs term < eps then
        sum, terms
    else
        sumUntil getTerm nextState (nextState state) (sum + term) (terms + 1)

let dumbTerm x n =
    pown x (4 * n) / pown 4.0 (n + 1)

let dumbTaylor x =
    sumUntil (dumbTerm x) (fun n -> n + 1) 0 0.0 0

let smartTaylor x =
    let ratio = pown x 4 / 4.0
    sumUntil id (fun term -> term * ratio) 0.25 0.0 0

let printHeader =
    printfn "f(x) = 1 / (4 - x^4), x in [%.1f; %.1f], eps = %.1e" a b eps
    printfn "%6s | %14s | %14s | %7s | %14s | %7s" "x" "Builtin" "Smart Taylor" "# terms" "Dumb Taylor" "# terms"
    printfn "-----------------------------------------------------------------------------------------------"

let rec printRows i =
    if i > segments then
        ()
    else
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

let printTable =
    printHeader
    printRows 0

printTable
