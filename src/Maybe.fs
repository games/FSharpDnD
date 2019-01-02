module Maybe

open System.Diagnostics

/// Maybe computation expression builder, copied from ExtCore library
/// https://github.com/jack-pappas/ExtCore/blob/master/ExtCore/Control.fs
[<Sealed>]
type MaybeBuilder () =
    // 'T -> M<'T>
    [<DebuggerStepThrough>]
    member inline __.Return value: 'T option = Some value

    // M<'T> -> M<'T>
    [<DebuggerStepThrough>]
    member inline __.ReturnFrom value: 'T option = value

    // unit -> M<'T>
    [<DebuggerStepThrough>]
    member inline __.Zero (): unit option = Some ()     // TODO: Should this be None?

    // (unit -> M<'T>) -> M<'T>
    [<DebuggerStepThrough>]
    member __.Delay (f: unit -> 'T option): 'T option = f ()

    // M<'T> -> M<'T> -> M<'T>
    // or
    // M<unit> -> M<'T> -> M<'T>
    [<DebuggerStepThrough>]
    member inline __.Combine (r1, r2: 'T option): 'T option =
        match r1 with
        | None -> None
        | Some () -> r2

    // M<'T> * ('T -> M<'U>) -> M<'U>
    [<DebuggerStepThrough>]
    member inline __.Bind (value, f: 'T -> 'U option): 'U option = Option.bind f value

    // 'T * ('T -> M<'U>) -> M<'U> when 'U :> IDisposable
    [<DebuggerStepThrough>]
    member __.Using (resource: ('T :> System.IDisposable), body: _ -> _ option): _ option =
        try body resource
        finally if not <| obj.ReferenceEquals (null, box resource) then resource.Dispose ()

    // (unit -> bool) * M<'T> -> M<'T>
    [<DebuggerStepThrough>]
    member x.While (guard, body: _ option): _ option =
        if guard () then
            // OPTIMIZE: This could be simplified so we don't need to make calls to Bind and While.
            x.Bind (body, (fun () -> x.While (guard, body)))
        else x.Zero ()

    // seq<'T> * ('T -> M<'U>) -> M<'U>
    // or
    // seq<'T> * ('T -> M<'U>) -> seq<M<'U>>
    [<DebuggerStepThrough>]
    member x.For (sequence: seq<_>, body: 'T -> unit option): _ option =
        // OPTIMIZE: This could be simplified so we don't need to make calls to Using, While, Delay.
        x.Using (sequence.GetEnumerator (), fun enum ->
            x.While (enum.MoveNext,
                x.Delay (fun () -> body enum.Current)
            )
        )

    member __.Either (m1 : 'T option) (m2 : 'T option) = 
        match m1 with
        | Some x -> Some x
        | None -> m2  

let maybe = MaybeBuilder()

module Option =
    let apply (f : ('a -> 'b) option) (v : 'a option) =
        Option.bind (fun f' ->
            Option.bind (fun v' ->
            Some (f' v')) v) f

module Operators =
    let (>>=) m f = Option.bind f m
    let (>=>) f1 f2 x = f1 x >>= f2
    let (>>%) m v = m >>= (fun _ -> maybe.Return v)
    let (>>.) m1 m2 = m1 >>= (fun _ -> m2)
    let (.>>) m1 m2 = m1 >>= (fun x -> m2 >>% x)
    let (.>>.) m1 m2 = m1 >>= (fun x -> m2 >>= (fun y -> maybe.Return (x, y)))
    let (<|>) m1 m2 = maybe.Either m1 m2
    let (<!>) = Option.map
    let (<*>) = Option.apply
    let (<?>) m v = Option.defaultValue v m
