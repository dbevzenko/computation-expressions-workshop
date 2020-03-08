module Sequences

// trait-call (SRTP)
let inline getName value = (^t : (member name : _) value)
// val inline getName : value: ^t -> 'a when  ^t : (member get_name :  ^t -> 'a)

let inline (.+) (v : ^a) (l : ^a list) = v :: l

open Expecto

type Stack<'a> =
    | Empty
    | Cons of top : 'a * rest : Stack<'a>

module Stack =

    /// Pushes a new value on top of the stack
    let push v s = Cons(v, s)

    /// Pops the top value off the stack,
    /// returning both the value and remaining stack.
    /// Throws an error if there are no remaining values.
    let pop s =
        match s with
        | Cons(v, rest) -> v, rest
        | _ -> failwith "Nothing to pop!"

    // foldl :: ('b -> 'a -> 'b) -> 'b -> Stack<'a> -> 'b
    let rec foldl (g: 'b -> 'a -> 'b) (v: 'b) (s: Stack<'a>) : 'b =
        // left associative operator
        // ((((v + 0) + 1) + 2) + 3)
        match s with
        | Cons(head, tail) ->
            foldl g (g v head) tail
        | Empty -> v

    // foldr :: ('a -> 'b -> 'b) -> 'b -> Stack<'a> -> 'b
    let rec foldr (g: 'a -> 'b -> 'b) (v: 'b) (s: Stack<'a>) : 'b =
        // right associative operator
        // (0 + (1 + (2 + (3 + v))))
        match s with
        | Cons(head, tail) ->
            g head (foldr g v tail)
        | Empty -> v

    let rec map (f: 'a -> 'b) (s: Stack<'a>) : Stack<'b> =
        match s with
        | Cons(head, tail) ->
            push (f head) (map f tail)
        | Empty -> Empty

    let append s1 s2 =
        let rec loop s1 cont =
            match s1 with
            | Cons(top, rem) ->
                loop rem (fun rest -> cont(Cons(top, rest)))
            | Empty -> cont s2
        loop s1 id

    /// Applies the function f to each element in the stack and concatenates the results.
    let collect (f:'a -> Stack<'b>) (m:Stack<'a>) =
        let rec loop s cont =
            match s with
            | Cons(top, rem) ->
                loop rem (fun rest -> cont(append (f top) rest))
            | Empty -> cont Empty
        loop m id

    /// Converts the Stack<'a> to an 'a list.
    //let toList s = foldr (fun v l -> v :: l) [] s
    let toList s = foldr (.+) [] s

    // sum stack elements
    let inline sum s = foldr (+) LanguagePrimitives.GenericZero s

    /// Pushes a value onto a new stack.
    let return' v = push v Empty

type StackBuilder() =
    member _.Yield x =
        printfn "stack.Yield(%A)" x
        Stack.return' x                                            // This Return!!!!
    member _.YieldFrom(m : Stack<'a>) =
        printfn "stack.YieldFrom(%A)" m
        m

    member _.Combine(m1 : Stack<'a>, m2 : Stack<'a>) =
        printfn "stack.Combine(%A, %A)" m1 m2
        // m2 is the value at the right
        // m1 is folded right-to-left and appended to the left
        // This makes Combine function like List.append
        Stack.foldr Stack.push m2 m1

    // This makes yield eagerly evaluated (kind of strict)
    //member __.Delay(f) = f()

    // This doesn't work either
    //member __.Combine(s1, s2) = Stack.append s1 (s2())
//    member __.Delay(f) = f
//    member __.Run(f) = f()

    // To get delayed execution (lazy, non-strict)
    member _.For(m, f) = Stack.collect f m                         // This is Bind !!!!!!!!!

    // Now that we have a `Bind` in the form of a `For` and `Return` in the form of `Yield`,
    // will the strategy used in `StateBuilder` now allow our builder to delay execution?
    //member _.Delay(f) = Stack.collect f (Stack.return' ()) // Compare with StateBuilder!!!!

    // Conclusion: Just like `'a list`, our `Stack` is eagerly evaluated to produce a result.
    // Attempting to block this with iteration fails. We would need to go to greater lengths and
    // create a true `LazyList` type using either `Lazy<'T>` or a `unit -> 'T` as the internal
    // type (see also lazy CE). Therefore the below Delay implementation is the easiest

    member __.Delay(f:unit -> Stack<'a>) = f()

let stack = StackBuilder()

[<Tests>]
let tests =
    testList "sequences" [

        test "Stack.toList generates a matching list" {
            let actual = Cons(1, Cons(2, Cons(3, Empty))) |> Stack.toList
            Expect.equal actual [1;2;3] "Expected list containing [1;2;3]"
        }

        test "Stack.map generates correct result" {
            let actual = Cons(1, Cons(2, Cons(3, Empty))) |> Stack.map (fun x -> x * x) |> Stack.toList
            Expect.equal actual [1;4;9] "Expected list containing [1;4;9]"
        }

        test "stack can return one item" {
            let actual = stack { yield 1 }
            Expect.equal (Stack.toList actual) [1] "Expected a stack containing 1"
        }

        test "stack can yield an empty stack" {
            let actual = stack { yield! Empty }
            Expect.equal actual Empty "Expected an empty stack"
        }

        test "stack can return multiple items" {
            let actual = stack {
                printfn "INFO: before yield 1"
                yield 1
                printfn "INFO: before yield 2"
                yield 2
                printfn "INFO: before yield 3"
                yield 3
            }
            Expect.equal (Stack.toList actual) [1;2;3] "Actual should match expected"
        }

        test "stack can iterate and yield" {
            let expected = stack { yield 1; yield 2; yield 3 }
            let actual = stack {
                for x in expected do
                    yield x
            }
            Expect.equal actual expected "Expected iterating and yielding to return the same result"
        }
    ]
