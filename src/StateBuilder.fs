module States

open Expecto

// The State Monad!
// ================
// We have an example of how to resolve a common F# design question, "Where do I put this common state parameter?"
// If you have a set of functions that work together, and you are trying to determine whether to put the state instance
// as the first parameter -- so you can partially apply it -- or as the last -- so you can pipe it -- to the several,
// related functions, you may just as well want a CE that allows you to define the computation and then feed the state
// at the end (think props in React).


/// State is a function type that takes a state and
/// returns a value and the new state.
type State<'TValue, 'TState> = 'TState -> 'TValue * 'TState


/// Single case union of the same type (if you prefer wrapping/unwrapping using pattern matching and return).
//type State2<'TValue, 'TState> = State of ('TState -> 'TValue * 'TState)

module State =
    // Explicit
    //let return' x : State<'a, 's> = fun s -> x, s
    // 'a -> ('s -> 'a * 's)
    // Less explicit but works better with other, existing functions:
    let result x s = x, s // create a state, with a given value, and where nextState is unchanged

    // bind : ('a -> State<'b, 's>) -> State<'a, 's> -> State<'b, 's>
    // bind : ('a -> ('s -> 'b * 's)) -> ('s -> 'a * 's) -> ('s -> 'b * 's)
    let bind (g: 'a -> State<'b, 's>) (m : State<'a, 's>) : State<'b, 's> =
        // 's -> 'b * 's
        let result = fun s ->
            // run the (existing) state function to get a value and a next state
            let v, s' = m s
            // get a new state computation by applying the monadic function (cross-world function)
            let m' = g v
            // apply the new computation to the next state
            m' s'
        result

    // NOTE: inline => SRTP
    //    combine : (m1 : State< ^a,'s>) -> (m2 : State< ^a,'s>) -> State< ^a,'s>
    //       when  ^a : (static member ( + ) :  ^a *  ^a ->  ^a)
    let inline combine (m1 : State<'a, 's>) (m2 : State<'a, 's>) : State<'a, 's> =
        // 's -> 'b * 's
        fun s ->
            let v1, s1 = m1 s
            let v2, s2 = m2 s
            // This is arbitrary (the correct thing to do depend on the exact concrete state monad)
            v1 + v2, s1 + s2

    //let run (m : State<'a, 's>) (s: 's) = m s // Only needed when single case union (or record) is used

    /// Evaluates the computation, returning the result value.
    let eval (m:State<'a, 's>) (s:'s) = m s |> fst

    /// Executes the computation, returning the final state.
    let exec (m:State<'a, 's>) (s:'s) = m s |> snd

    /// Returns the state as the value.
    let getState (s:'s) = s, s

    /// Ignores the state passed in favor of the provided state value.
    let setState (s:'s) = fun _ -> (), s

let return' (x: 'a) : State<'a, 's> = State.result x

let (>>=) m f = State.bind f m

type StateBuilder() =
    member _.Return(value) : State<'a, 's> =
        State.result value
    member _.Bind(m: State<'a, 's>, f: 'a -> State<'b, 's>) : State<'b, 's> =
        State.bind f m
    member _.ReturnFrom(m:State<'a, 's>) =
        m
    member _.Zero() = State.result ()

    // Get non-strict evaluation
    //member __.Delay(f) = f
    //member __.Run(f) = f()

    member __.Delay(f) = State.bind f (State.result ()) // This will imply non-strict semantics

    // NOTE: We have to inline this to
    member inline _.Combine(m1 : State<'a, 's>, m2 : State<'a, 's>) : State<'a, 's> = State.combine m1 m2

//    member __.Combine(m1 : State<unit, 's>, m2 : State<'a, 's>) =
//        // Carry state from m1 through to m2 while ignoring the value from m1.
//        State.bind (fun () -> m2) m1

//    member inline _.Combine(m1:State<'a, 's>, m2:State<'a, 's>) =
//        fun s ->
//            let v1, s1 = m1 s
//            let v2, s2 = m2 s
//            v1 + v2, s1 + s2

let state = StateBuilder()

open System.Text

[<Tests>]
let tests =
    testList "states" [
        test "StringBuilder as state but without CE" {
            // StringBuilder -> StringBuilder functions
            let printA (sb:StringBuilder) = sb.Append("A")
            let printB (sb:StringBuilder) = sb.Append("B")
            let printC (sb:StringBuilder) = sb.Append("C")

            // StringBuilder -> string
            let run (sb:StringBuilder) = sb.ToString()

            // Usage
            let sb = StringBuilder()
            let actual = sb |> printA |> printB |> printC |> run

            Expect.equal actual "ABC" "Expected ABC."
        }

        test "returns value" {
            // The State monad is a computation (like a parser)
            let reverseStringComputation = state {
                // get the input
                let! (s : string) = State.getState
                //pure
                return System.String(s.ToCharArray() |> Array.rev)
            }
            // calculate the value
            let actual = State.eval reverseStringComputation "Hello"
            Expect.equal actual "olleH" "Expected \"olleH\" as the value."
        }

        test "returns without changing state" {
            let c = state {
                let! (s : string) = State.getState
                return System.String(s.ToCharArray() |> Array.rev)
            }
            let actual = State.exec c "Hello"
            Expect.equal actual "Hello" "Expected \"Hello\" as the state."
        }

        test "returns unit" {
            let c = state {
                let! (s : string) = State.getState
                let s' = System.String(s.ToCharArray() |> Array.rev)
                do! State.setState s'
            }
            // returns unit, because setState works this (odd) way
            let actual = State.eval c "Hello"
            Expect.equal actual () "Expected return value of unit."
        }

        test "returns changed state" {
            let c = state {
                let! (s : string) = State.getState
                let s' = System.String(s.ToCharArray() |> Array.rev)
                do! State.setState s'
            }
            // returns the result of the computation via the state, because of setState
            let actual = State.exec c "Hello"
            Expect.equal actual "olleH" "Expected state of \"elloH\"."
        }

        test "state supports if ... then with no else if Zero is defined" {
            let c : State<unit, string> = state {
                if true then
                    printfn "Hello"
            }
            let actual = State.eval c ""
            Expect.equal actual () "Expected the value to be ()."
        }

        test "state supports returning multiple values" {
            let c : State<string, string> = state {
                return "one"
                return "two"
            }
            let actual = State.eval c ""
            Expect.equal actual "onetwo"  "Expected all returns to be concatenated."
        }
    ]