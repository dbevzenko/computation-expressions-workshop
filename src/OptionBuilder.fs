module Options

open Expecto

module Option =
    // (m a -> m b) -> m a -> m b
    let apply g x =
        match g, x with
        | Some f, Some y -> Some <| f y
        | _, _ -> None

    let bindM(m, f) = Option.bind f m

    let empty = None

let return' x = Some x

let (<*>) = Option.apply
let (<!>) = Option.map

let (>>=) x f = Option.bind f x

let opt1 = Some 1
let opt2 = Some 2
let opt3 = Some 3
let opt4 = Some 4

let sum4 w x y z = w + x + y + z

let nested =
    match opt1 with
    | Some w ->
        match opt2 with
        | Some x ->
            match opt3 with
            | Some y ->
                match opt4 with
                | Some z ->
                    let result = sum4 w x y z
                    printfn "Nested: %d" result
                    Some result
                | None -> None
            | None -> None
        | None -> None
    | None -> None

let composed =
    opt1
    |> Option.bind (fun w ->
        opt2
        |> Option.bind (fun x ->
            opt3
            |> Option.bind (fun y ->
                opt4
                |> Option.map (fun z ->
                    let result = sum4 w x y z
                    printfn "Composed: %d" result
                    result
                )
            )
        )
    )

let composedApp = sum4 <!> opt1 <*> opt2 <*> opt3 <*> opt4

let composedBind = opt1 >>= (fun x ->
                   opt2 >>= (fun y ->
                   opt3 >>= (fun z ->
                   opt4 >>= (fun v ->
                   return' <| sum4 x y z v))))

// This is what the CE is transformed into. Uses tuple params.
let composedBindNonInfix =
    Option.bindM(opt1, fun w ->
        Option.bindM(opt2, fun x ->
            Option.bindM(opt3, fun y ->
                Option.bindM(opt4, fun z ->
                    let result = sum4 w x y z
                    return' result
                )
            )
        )
    )


// NOTE: builder uses tuple params (overloaded members)

/// maybe monad
type OptionBuilder() =
    // pure, return, unit
    member _.Return x =
        printfn "maybe.Return(%A)" x
        Some x
    member _.ReturnFrom x =
        printfn "maybe.ReturnFrom(%A)" x
        x
    /// bind
    member _.Bind(x, f) =
        printfn "maybe.Bind(%A, %A)" x f
        Option.bind f x
    /// empty of Alternative (Alternative is a subclass of Applicative: empty and <|> --- a.k.a choice)
    /// empty is an applicative computation with zero results
    /// while (<|>) is a binary function which combines two computations.
    member _.Zero() =
        printfn "maybe.Zero()"
        None               // This is required in order to define CE without explicitly returning a value

let maybe = OptionBuilder()

[<Tests>]
let tests =
    testList "OptionBuilder" [

        test "OptionBuilder returns value" {
            let expected = 1
            // This control construct may only be used if the computation expression builder defines a 'Return' method
            let actual = maybe { return expected }
            Expect.equal actual (Some expected) "Expected Some 1"
        }

        test "OptionBuilder can bind option values" {
            let actual = maybe {
                // This control construct may only be used if the computation expression builder
                // defines a 'Bind' method
                let! x = opt1
                let! y = opt2
                let! z = opt3
                let! v = opt4
                let result = sum4 x y z v
                return result
            }
            Expect.equal actual nested "Actual should sum to the same value as nested."
        }

        test "OptionBuilder maps value" {
            let actual = maybe {
                let! x = Some 1
                return x
            }
            Expect.equal actual (Some 1) "Expected Some 1"
        }

        test "OptionBuilder can exit without returning a value" {

            let fullyQualified path =
                let fileInfo = System.IO.FileInfo(path)
                let fileName = fileInfo.Name
                let pathDir = fileInfo.Directory.FullName.TrimEnd('~')
                if System.IO.Directory.Exists(pathDir) then
                    Some (System.IO.Path.Combine(pathDir, fileName))
                else None

            let maybePath = Some "~/test.txt"

            let actual =
                maybe {
                    let! path = maybePath
                    let! fullPath = fullyQualified path
                    // This control construct may only be used if the computation expression builder defines a 'Zero' method
                    System.IO.File.WriteAllText(fullPath, "Test succeeded")
                }

            Expect.equal actual None "Actual should be None"
        }

        test "OptionBuilder supports if then without an else" {
            let maybePath = Some "~/test.txt"

            let actual =
                maybe {
                    let! path = maybePath
                    let pathDir = System.IO.Path.GetDirectoryName(path)
                    if not (System.IO.Directory.Exists(pathDir)) then
                        return "Select a valid path."
                    // maybe.Zero is returned here on else path
                }

            Expect.equal actual (Some "Select a valid path.") "Actual should return Some(\"Select a valid path.\")"
        }

        test "OptionBuilder allows for early escape with return!" {
            let actual =
                maybe {
                    if true then
                        // This control construct may only be used if the computation expression builder defines a 'ReturnFrom' method
                        return! None
                    else
                        let! w = opt1
                        let! x = opt2
                        let! y = opt3
                        let! z = opt4
                        let result = sum4 w x y z
                        printfn "Result: %d" result // print if a result was computed.
                        return result
                }
            Expect.equal actual None "Should return None immediately"
        }
    ]

// Builder API (signatures)
//
// type AsyncBuilder with
//     member For: seq<'T> * ('T -> Async<unit>) -> Async<unit>
//     member Zero : unit -> Async<unit>
//     member Combine : Async<unit> * Async<'T> -> Async<'T>
//     member While : (unit -> bool) * Async<unit> -> Async<unit>

//     member Return : 'T -> Async<'T>
//     member Bind: Async<'T> * ('T -> Async<'U>) -> Async<'U>

//     member Delay : (unit -> Async<'T>) -> Async<'T>

//     member Using: 'T * ('T -> Async<'U>) -> Async<'U>

//     when 'U :> System.IDisposable

//     member TryFinally: Async<'T> * (unit -> unit) -> Async<'T>
//     member TryWith: Async<'T> * (exn -> Async<'T>) -> Async<'T>


// The following expression forms are all computation expressions:
//    expr { for ... }
//    expr { let ... }
//    expr { let! ... }
//    expr { use ... }
//    expr { while ... }
//    expr { yield ... }
//    expr { yield! ... }
//    expr { try ... }
//    expr { return ... }
//    expr { return! ... }
// More specifically, computation expressions have the following form:
//    builder-expr { cexpr }


// Syntax
//
// comp-expr :=
//     let! pat = expr in comp-expr              -- binding computation
//     let pat = expr in comp-expr
//     do! expr in comp-expr                     -- sequential computation
//     do expr in comp-expr
//     use! pat = expr in comp-expr              -- auto cleanup computation
//     use pat = expr in comp-expr
//     yield! expr                             -- yield computation
//     yield expr                              -- yield result
//     return! expr                            -- return computation
//     return expr                             -- return result
//     if expr then comp-expr                  -- control flow or imperative action
//     if expr then expr else comp-expr
//     match expr with pat -> comp-expr | … | pat -> comp-expr
//     try comp-expr with pat -> comp-expr | … | pat -> comp-expr
//     try comp-expr finally expr
//     while expr do comp-expr done
//     for ident = expr to expr do comp-expr done
//     for pat in expr-or-range-expr do comp-expr done
//     comp-expr ; comp-expr
//     expr
//
//
// short-comp-expr :=
//    for pat in expr-or-range-expr -> expr     -- yield result (isn't this lst comprehension?)