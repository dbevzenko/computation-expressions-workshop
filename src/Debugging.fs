module Debugging

// See also https://github.com/dotnet/fsharp/pull/4867
//        -- and --
// https://github.com/fsharp/fslang-design/blob/master/FSharp.Core-4.5.0.0/FS-1059-improve-async-stack-traces.md
module DSL =
    // delayed computation (monad)
    type Computation<'T> = C of (unit -> 'T)

    let return' x = C (fun () -> x)

    let bind (C f) k = C(fun () -> let (C f2) = k (f ()) in f2())

    let add c1 c2 = bind c1 (fun v1 -> bind c2 (fun v2 -> return' (v1 + v2)))

    let run (C f) = f ()

    let delay f = bind (return' ()) f

    let stack (msg : string) c1 =
        delay (fun () ->
            System.Console.WriteLine("----{0}-----\n{1}\n----------", msg, System.Diagnostics.StackTrace(true).ToString())
            c1)

// The third option, and the one used here, is to "inline" the key parts of the computational
// language (and its composition operations) so that the key closures which implement the composition
// become user code. This relies on the fact that when the F# compiler inlines code, it re-tags closures
// contained in that code with the source information of callsite.

// The rules we've applied are:
//
// 1. Inline the mechanisms of composition
// 2. Expose any necessary "runtime" functions such as apply and mark them DebuggerHidden

open Expecto
open DSL

[<Tests>]
let tests =
    testList "debugging CE" [

        // Now the "stack" when the "return' 6" computation of f1() is actually
        // "invoked" (i.e. when it gets invoked as part of f3() |> run) is bad.
        // Barely any user code is on the stack at all, and the line numbers are all closures
        // in the implementation of the DSL.
        //
        // It is also important to note that C# async and other implementations of
        // Promises/Tasks/"Hot Tasks" don't suffer this problem since they are not "delayed".
        // But most F# DSLs are "delayed".
        // For async { }, it is F#'s use of "cold tasks" that require explicit starting that
        // causes the issue.
        test "test" {
            // define 3 delayed computations

        //    ----F1-----
        //   at Debugging.DSL.stack@21.Invoke(Unit unitVar0) in C:\Dev\FP\computation-expressions-workshop\src\Debugging.fs:line 22
        //   at Debugging.DSL.bind@12.Invoke(Unit unitVar0) in C:\Dev\FP\computation-expressions-workshop\src\Debugging.fs:line 12
        //   at Debugging.DSL.bind@12.Invoke(Unit unitVar0) in C:\Dev\FP\computation-expressions-workshop\src\Debugging.fs:line 12
        //   at Debugging.DSL.run[a](Computation`1 _arg1) in C:\Dev\FP\computation-expressions-workshop\src\Debugging.fs:line 16
        //   at Debugging.tests@55.Invoke(Unit unitVar) in C:\Dev\FP\computation-expressions-workshop\src\Debugging.fs:line 59

            // define delayed actions
            let f1() = return' 6
            //let f1() = stack "F1" (return' 6)
            let f2() = return' 7
            // compose/combine
            let f3() = add (f1()) (f2())

            // run the composite delayed computation
            let result = f3() |> run
            printfn "result = %A" result

        }

    ]