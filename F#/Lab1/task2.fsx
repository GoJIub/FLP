let eps = 1e-10
let maxIterations = 10000

type SolveResult =
    | Converged of float * int
    | Diverged of int
    | DomainError of string

let dichotomy f a b =
    let fa = f a

    if fa * f b > 0.0 then
        DomainError "Dichotomy requires opposite signs on [a, b]"
    else

        let rec loop left right fLeft iter =
            let mid = (left + right) / 2.0
            let fMid = f mid

            if abs fMid < eps || abs (right - left) / 2.0 < eps then
                Converged (mid, iter + 1)
            elif iter >= maxIterations then
                Diverged (iter + 1)
            elif fLeft * fMid <= 0.0 then
                loop left mid fLeft (iter + 1)
            else
                loop mid right fMid (iter + 1)

        loop a b fa 0

let iterations phi x0 =
    let rec loop x iter =
        let next = phi x

        if abs (next - x) < eps then
            Converged (next, iter + 1)
        elif iter >= maxIterations then
            Diverged (iter + 1)
        else
            loop next (iter + 1)

    loop x0 0

let newton f derivative x0 =
    let phi x =
        let d = derivative x

        if abs d < 1e-14 then
            nan
        else
            x - f x / d

    iterations phi x0

let formatCell result =
    match result with
    | Converged (root, iterations) -> sprintf "%.8f (%d)" root iterations
    | Diverged iterations -> sprintf "DIVERGED (%d)" iterations
    | DomainError reason -> sprintf "ERROR: %s" reason

let printRow number dResult iResult nResult =
    let dCell = formatCell dResult
    let iCell = formatCell iResult
    let nCell = formatCell nResult

    printfn
        "%-4d | %28s | %28s | %28s"
        number
        dCell
        iCell
        nCell

let f25 x =
    sqrt (1.0 - 0.4 * x * x) - asin x

let f26 x =
    exp x - exp (-x) - 2.0

let f27 x =
    sin (log x) - cos (log x) + 2.0 * log x

let f25' x =
    -0.4 * x / sqrt (1.0 - 0.4 * x * x) - 1.0 / sqrt (1.0 - x * x)

let f26' x =
    exp x + exp (-x)

let f27' x =
    (cos (log x) + sin (log x) + 2.0) / x

let phi25 x =
    sin (sqrt (1.0 - 0.4 * x * x))

let phi26 x =
    log (2.0 + exp (-x))

let phi27 x =
    x - 0.4 * f27 x

let solveOne f f' phi a b x0 =
    let dResult = dichotomy f a b
    let iResult = iterations phi x0
    let nResult = newton f f' x0
    dResult, iResult, nResult

let solveVariant25 =
    solveOne f25 f25' phi25 0.0 1.0 0.5

let solveVariant26 =
    solveOne f26 f26' phi26 0.0 1.0 0.5

let solveVariant27 =
    solveOne f27 f27' phi27 1.0 3.0 1.5

printfn "%-4s | %28s | %28s | %28s" "#" "Dichotomy" "Iterations" "Newton"
printfn "------------------------------------------------------------------------------------------------------"

let d25, i25, n25 = solveVariant25
let d26, i26, n26 = solveVariant26
let d27, i27, n27 = solveVariant27

printRow 25 d25 i25 n25
printRow 26 d26 i26 n26
printRow 27 d27 i27 n27
