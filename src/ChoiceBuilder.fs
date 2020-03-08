module Choices

open Options
open Expecto

/// None coalescing operator for Option<'T>
let (<|>) x y =
    match x, y with
    | None, None -> None
    | Some _, None -> x
    | None, Some _ -> y
    | Some _, Some _ -> x

// Is this Alternative in Haskell (Zero, Combine)???
type ChoiceBuilder() =
    //inherit OptionBuilder()

    member _.Zero() =
        printfn "choose.Zero()"
        None               // This is required in order to define CE without explicitly returning a value

    // return
    member _.Return x =
        printfn "choose.Return(%A)" x
        Some x

    // let!
    member _.Bind(x, f) =
        printfn "choose.Bind(%A, %A)" x f
        Option.bind f x

    // return!
    member _.ReturnFrom x =
        printfn "choose.ReturnFrom(%A)" x
        x

//    member _.Combine(m1: 'a option, m2: 'a option) =
//        printfn "choose.Combine(%A, %A)" m1 m2
//        // equivalent to <|>, why isn't Combine called Choice?
//        match m1 with
//        | Some _ -> m1
//        | None -> m2

//    member _.Delay(f: unit -> 'a option) =
//        printfn "choose.Delay(%A)" f
//        // immediate execution (no delay)
//        f()

    member _.Delay(f:unit -> 'a option) =
        printfn "choose.Delay(%A)" f
        // delaying execution
        f

    member _.Run(f:unit -> 'a option) =
        printfn "choose.Run(%A)" f
        // execution at return time
        f()

        // member Combine : unit option * 'a option) -> 'a option
//    member _.Combine(e1, e2) =
//        e1 |> Option.bind (fun () -> e2)

    // Combine that does not evaluate the following code that is delayed
    member _.Combine(m: 'a option, f: unit -> 'a option) =
        printfn "choose.Combine(%A, %A)" m f
        match m with
        | Some _ -> m
        | None -> f() //execute delay computation here

//    member _.Combine(m1: 'a option, m2: 'a option) =
//        printfn "choose.Combine(%A, %A)" m1 m2
//        m1 <|> m2

let choose = ChoiceBuilder()

[<Tests>]
let tests =
    testList "choices" [

        // This one logs
        //
        // choose.Delay(<fun:actual@153-12>)
        // choose.ReturnFrom(Some 1)
        // choose.Delay(<fun:actual@154-13>)
        // returning first value
        // choose.ReturnFrom(Some 2)
        // choose.Combine(Some 1, Some 2)
        test "choose returns first value if it is Some" {
            // Without Run: The actual values is now a delayed value 'unit -> int option'
            // With Run: The actual value is again an 'int option'
            let actual = choose {
                // This control construct may only be used if the computation expression builder defines a 'Combine' method
                return! Some 1
                printfn "returning first value"
                return! Some 2
            }
            // Spec: If the inferred type of b has one or more of the Run, Delay, or Quote methods when builder-expr is
            // checked, the translation involves those methods.
            //
            // let b = builder-expr in b.Run (<@ b.Delay(fun () -> {| cexpr |}C) >@)
            //
            // Without Quote
            // let b = builder-expr in b.Run (b.Delay(fun () -> {| cexpr |}C))
            //
            // Without Delay and Quote
            // let b = builder-expr in b.Run ({| cexpr |}C)

            // From the specification, `Run`, if implemented, is wrapped around the computation expression
            // and can be used to do almost anything.
            Expect.equal actual (Some 1) "Expected the first value to be returned."
        }

        test "choose returns second value if first one is None" {
            let actual = choose {
                // This control construct may only be used if the computation expression builder defines a 'Combine' method
                return! None
                return! Some 2
            }
            Expect.equal actual (Some 2) "Expected the second value to be returned."
        }

        test "choose returns Zero=Empty=None if all values are None" {
            let actual = choose {
                // This control construct may only be used if the computation expression builder defines a 'Combine' method
                return! None
                return! None
            }
            Expect.equal actual None "Expected the None to be returned."
        }

        test "choose returns second value if first is None" {
            let actual = choose {
                return! None
                printfn "returning second value?"
                return! Some 2
            }
            Expect.equal actual (Some 2) "Expected the second value to be returned."
        }

        test "choose returns the last value if all previous are None" {
            let actual = choose {
                return! None
                return! None
                return! None
                return! None
                return! None
                return! None
                return! Some 7
            }
            Expect.equal actual (Some 7) "Expected the seventh value to be returned."
        }

//        test "ChoiceBuilder can chain a computation onto another returning None, where None indicates success" {
//
//            // Get the full path if container directory exists
//            // string -> string option
//            let dirExists path =
//                let fileInfo = System.IO.FileInfo(path)
//                let fileName = fileInfo.Name
//                let pathDir = fileInfo.Directory.FullName.TrimEnd('~')
//                if System.IO.Directory.Exists(pathDir) then
//                    Some (System.IO.Path.Combine(pathDir, fileName))
//                else None
//
//            let choosePath = Some "~/test.txt"
//
//            // This will return Zero = None to indicate success
//            let writeFile =
//                maybe {
//                    let! path = choosePath
//                    let! fullPath = dirExists path
//                    // This returns Zero, because of missing return/return!
//                    System.IO.File.WriteAllText(fullPath, "Test succeeded")
//                }
//
//            // This will return 'Some "Successfully wrote file"'
//            let actual =
//                choose {
//                    return! writeFile
//                    return "Successfully wrote file"
//                }
//
//            Expect.equal actual (Some "Successfully wrote file") "Actual should indicate success"
//        }
    ]


