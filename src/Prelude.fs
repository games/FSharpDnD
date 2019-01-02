module FSharpDnD.Prelude

let findInTable (defaultValue : 'V) (predicate : 'K -> bool) (table : ('K * 'V) list) =
    table
    |> List.tryFind (fst >> predicate)
    |> Option.map snd
    |> Option.defaultValue defaultValue


type Dice = Dice of side:int    

let d0 = Dice 0
let d6 = Dice 6
let d8 = Dice 8
let d10 = Dice 10
let d20 = Dice 20

let random = new System.Random()

let rollOnce (Dice side) =
    if side = 0 then 0
    else random.Next(1, side + 1)

let roll times (dice : Dice) =
    let (Dice side) = dice
    if side = 0 then 0
    else
        let roll _ = rollOnce dice
        Seq.init times roll
        |> Seq.sum
