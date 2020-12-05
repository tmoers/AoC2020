namespace global

open System

[<RequireQualifiedAccess>]
module Option =

    let panic e = function
        | Some a -> a
        | None -> failwith e

    let ofResult = function
        | Ok x -> Some x
        | Error _ -> None

[<AutoOpen>]
module OptionComputationExpression =

    type OptionBuilder() =
        member _.Return(x) = Some x
        member _.Bind(x, f) = Option.bind f x

        member _.ReturnFrom(x) = x
        member this.Zero() = this.Return ()

        member _.Delay(f) = f
        member _.Run(f) = f()

        member this.While(guard, body) =
            if not (guard())
            then this.Zero()
            else this.Bind( body(), fun () ->
                this.While(guard, body))

        member this.TryWith(body, handler) =
            try this.ReturnFrom(body())
            with e -> handler e

        member this.TryFinally(body, compensation) =
            try this.ReturnFrom(body())
            finally compensation()

        member this.Using(disposable:#System.IDisposable, body) =
            let body' = fun () -> body disposable
            this.TryFinally(body', fun () ->
                match disposable with
                    | null -> ()
                    | disp -> disp.Dispose())

        member this.For(sequence:seq<_>, body) =
            this.Using(sequence.GetEnumerator(),fun enum ->
                this.While(enum.MoveNext,
                    this.Delay(fun () -> body enum.Current)))

        member this.Combine (a,b) =
            this.Bind(a, fun () -> b())

    let optional = new OptionBuilder()

[<RequireQualifiedAccess>]
module Result =

    let map = Result.map

    let apply fR xR =
        match fR, xR with
        | Ok f, Ok x -> Ok (f x)
        | Error err1, Ok _ -> Error err1
        | Ok _, Error err2 -> Error err2
        | Error err1, Error _ -> Error err1

    let private (<!>) = map
    let private (<*>) = apply

    let ofOption errorValue opt =
        match opt with
        | Some v -> Ok v
        | None -> Error errorValue

    let ofOptionWith f opt =
        match opt with
        | Some v -> Ok v
        | None -> Error (f ())

    let panic f = function
        | Ok a -> a
        | Error e -> e |> f |> failwith

    let ifError defaultVal = function
        | Ok x -> x
        | Error _ -> defaultVal

    let ifErrorWith f = function
        | Ok x -> x
        | Error e -> f e

    let sequence aListOfResults =
        let cons head tail = head::tail
        let consR headR tailR = cons <!> headR <*> tailR
        let initialValue = Ok []
        List.foldBack consR aListOfResults initialValue


[<AutoOpen>]
module ResultComputationExpression =

    type ResultBuilder() =
        member _.Return(x) = Ok x
        member _.Bind(x, f) = Result.bind f x

        member _.ReturnFrom(x) = x
        member this.Zero() = this.Return ()

        member _.Delay(f) = f
        member _.Run(f) = f()

        member this.While(guard, body) =
            if not (guard())
            then this.Zero()
            else this.Bind( body(), fun () ->
                this.While(guard, body))

        member this.TryWith(body, handler) =
            try this.ReturnFrom(body())
            with e -> handler e

        member this.TryFinally(body, compensation) =
            try this.ReturnFrom(body())
            finally compensation()

        member this.Using(disposable:#System.IDisposable, body) =
            let body' = fun () -> body disposable
            this.TryFinally(body', fun () ->
                match disposable with
                    | null -> ()
                    | disp -> disp.Dispose())

        member this.For(sequence:seq<_>, body) =
            this.Using(sequence.GetEnumerator(),fun enum ->
                this.While(enum.MoveNext,
                    this.Delay(fun () -> body enum.Current)))

        member this.Combine (a,b) =
            this.Bind(a, fun () -> b())

    let result = new ResultBuilder()
