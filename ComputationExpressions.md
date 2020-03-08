
Keywords used in CEs

```fsharp
expr { let! ... }      // Bind
expr { do! ... }       // 
expr { yield ... }
expr { yield! ... }
expr { return ... }
expr { return! ... }
expr { match! ... }
```

Thw keywords are only defined in a CE, if the builder type implements the corresponding API

Here is an API overview

```fsharp
let! is defined by the Bind(x, f) member on the builder type.

do! is defined by the Bind(x, f) member on the builder type, where f produces a unit.

yield is defined by the Yield(x) member on the builder type, where x is the item to yield back.

yield! is defined by the YieldFrom(x) member on the builder type, where x is a collection of values.

return is defined by the Return(x) member on the builder type, where x is the item to wrap.

return! is defined by the ReturnFrom(x) member on the builder type, where x is another computation expression.

The match! keyword is syntactic sugar for a let! binding followed by a pattern match on the bound expression
 
```

The exact signatures are defined, only the arity of parameters and the member names. Also the fact
that parameters are tuples (method overloading in F#).

The builder type is an object that defines special methods that govern the way the fragments of the computation expression are combined

## Idiomatic Builder API

The builder type behind a computation expression _do not_ adhere to strict or rigid type 
signatures but can be implemented to solve different problems depending on your use case.

Anyway the Builder API adhere to something like the following skeleton CE builder

```fsharp
// simple wrapper (could be anything of kind :: '* -> *')
type M<'T> = M of T' 

type BasicBuilder() =
    
    //
    // Monad, Applicative and Functor API
    //

    // Called for let! and do! in computation expressions.
    // M<'T> * ('T -> M<'U>) -> M<'U>
    member _.Bind(m: M<'T>, f: 'T -> M<'U>) : M<'U> = ...
    
    // Called for return in computation expressions.
    // 'T -> M<'T>
    member _.Return(x: 'T) : M<'T> = ...

    // Called for return! in computation expressions.
    // M<'T> -> M<'T>
    member _.ReturnFrom(m: M<'T>) : M<'T> = ...

    //
    // Choice/Combine API
    //

    // Called for empty else branches of if...then expressions in computation expressions.
    // unit -> M<'T>
    member _.Zero() = ...

    // NOTE: The signature could diverge, if delayed computation is needed
    // M<'T> * M<'T> -> M<'T> 
    //     -- or --
    // M<unit> * M<'T> -> M<'T>
    //     -- or --
    // M<'T> * (unit -> M<'T>) -> M<'T>
    member _.Combine(m1: 'a option, m2: 'a option) =

    //
    // IDisposable bindings API (monadic bind with dispose of resource)
    //

    // Called for use bindings in computation expressions.
    // 'T * ('T -> M<'U>) -> M<'U> when 'T :> IDisposable
    member _.Using(x, f) = ...

    //
    // List Comprehension (Seq) API
    //

    // Called for for...do expressions in computation expressions.
    // seq<'T> * ('T -> M<'U>) -> M<'U>
    //   -- or --
    // seq<'T> * ('T -> M<'U>) -> seq<M<'U>>
    // TODO: insert example

    // Called for yield expressions in computation expressions.
    // 'T -> M<'T>	
    member _.Yield x = ...

    // Called for yield! expressions in computation expressions.
    // M<'T> -> M<'T>	
    member _.YieldFrom m = ...

    // Called for while...do expressions in computation expressions.
    // (unit -> bool) * M<'T> -> M<'T>
    // member _.While(p, m) = ...

    // Called for for...do expressions in computation expressions.
    // seq<'T> * ('T -> M<'U>) -> M<'U>
    //   -- or ---
    // seq<'T> * ('T -> M<'U>) -> seq<M<'U>>
    //   -- or ---
    // M<'T> -> ('T -> M<'U>) -> M<'U>
    member _.For(m, f) = ...

    //
    // Try, Finally API
    //

    // Called for try...finally expressions in computation expressions.
    // M<'T> * (unit -> unit) -> M<'T>
    member _.TryFinally(m, f) = ...

    // Called for try...with expressions in computation expressions.
    // M<'T> * (exn -> M<'T>) -> M<'T>
    member _.TryWith(m, e) = ...

    //
    // Delay, Run, Quote wrappers of CE's
    //

    // Wraps a computation expression as a function.
    // (unit -> M<'T>) -> M<'T>
    member _.Delay(f: unit -> M<'T>) =

    // Executes a computation expression.
    // M<'T> -> M<'T>
    //   -- or --
    // M<'T> -> 'T
    member _.Run(f: unit -> M<'T>) =
	
    // Indicates that the computation expression is passed to the Run member as a quotation. It translates all instances of a computation into a quotation.
    // Quotations.Expr<'T> -> Quotations.Expr<'T>
    // TODO: Insert member
```

The F# compiler will convert all the small expressions into a series of nested 
function calls by using the methods in the preceding table and the code in the 
computation expression.

The nested expression is of the following form:

```fsharp
builder.Run(builder.Delay(fun () -> {| cexpr |}))
```

In the above code, the calls to Run and Delay are omitted if they are not defined in the computation expression builder class.

The body of the computation expression, here denoted as `{| cexpr |}`, is 
translated into calls involving the methods of the builder class by the translations described 
in the following table. The computation expression `{| cexpr |}` is defined recursively according 
to these translations where expr is an F# expression and `cexpr` is a computation expression.


| `cexpr`      | Transformed into   | 
| ------------- | ------------- | 
| `{ let binding in cexpr }`     | `let binding in {\| cexpr \|}` | 
| `{ let! pattern = expr in cexpr }`     | `builder.Bind(expr, (fun pattern -> {\| cexpr \|}))` | 
| `{ do! expr in cexpr }`     | `builder.Bind(expr, (fun () -> {\| cexpr \|}))` | 
| `{ yield expr }`     | `builder.Yield(expr)` | 
| `{ yield! expr }`     | `builder.YieldFrom(expr)` | 
| `{ return expr }	`     | `	builder.Return(expr)` | 
| `{ return! expr }`     | `builder.ReturnFrom(expr)` | 
| `{ use pattern = expr in cexpr }`     | `builder.Using(expr, (fun pattern -> {\| cexpr \|}))` | 
| `{ use! value = expr in cexpr }`     | `builder.Bind(expr, (fun value -> builder.Using(value, (fun value -> { cexpr }))))` | 
| `{ if expr then cexpr0 }`     | `if expr then { cexpr0 } else builder.Zero()` | 
| `{ if expr then cexpr0 else cexpr1 }`     | `if expr then { cexpr0 } else { cexpr1 }` | 
| `{ match expr with \| pattern_i -> cexpr_i }`     | `match expr with \| pattern_i -> { cexpr_i }` | 
| `{ for pattern in expr do cexpr }`     | `builder.For(enumeration, (fun pattern -> { cexpr }))` | 
| `{ for identifier = expr1 to expr2 do cexpr }`     | `builder.For(enumeration, (fun identifier -> { cexpr }))` | 
| `{ while expr do cexpr }`     | `builder.While(fun () -> expr, builder.Delay({ cexpr }))` | 
| `{ try cexpr with \| pattern_i -> expr_i }`     | `builder.TryWith(builder.Delay({ cexpr }), (fun value -> match value with \| pattern_i -> expr_i \| exn -> reraise exn)))` | 
| `{ try cexpr finally expr }`     | `builder.TryFinally(builder.Delay( { cexpr }), (fun () -> expr))` | 
| `{ cexpr1; cexpr2 }`     | `builder.Combine({ cexpr1 }, { cexpr2 })` | 
| `{ other-expr; cexpr }`     | `expr; { cexpr }` | 
| `{ other-expr }`     | `expr; builder.Zero()` | 
 
