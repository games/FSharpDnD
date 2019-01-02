module FSharpDnD.Combat

open System
open Prelude
open Types
open Core
open Maybe

type Combatant = 
    { Character : Character
      Party : Party 
      Roll : int }
and CombatState = 
    { TurnList : Combatant list
      AttackResults : AttackResults list }

let empty =
    { TurnList = []; AttackResults = [] }

let determineSurprise () =
    None

let establishPositions (characters : Character list) =
    characters

let initiative (parties : Party list) =
    let rollInitiative (character : Character) =
        rollOnce d20 + character.Abilities.Dexterity + character.AbilityModifiers.Dexterity
    let toCombatant (party : Party) (character : Character) =
        { Character = character
          Party = party
          Roll = rollInitiative character }
    parties
    |> List.collect (fun x -> List.map (toCombatant x) x.Members)
    |> List.sortByDescending (fun x -> x.Roll)


let attack (weapon : Weapon option) (attacker : Character) (target : Character) : AttackResults =
    let attackRoll = rollOnce d20
    let miss = attackRoll = 1
    let criticalHit = attackRoll = 20
    if miss then Miss (attacker, [ target ])
    else
        let (weaponType, damageDice, damageType) =
            match weapon with
            | Some weapon -> weapon.Type, weapon.DamageDice, weapon.DamageType
            | None -> SimpleMelee, d0, DamageTypes.Nil
        let modifier = 
            match weaponType with
            | SimpleMelee | MartialMelee -> attacker.AbilityModifiers.Strength
            | SimpleRanged | MartialRanged -> attacker.AbilityModifiers.Dexterity
        let proficiency = 
            if List.contains weaponType attacker.ClassFeatures.Proficiencies.Weapons then
                Character.proficiencyBonus attacker.Stats.Level
            else 0
        let attackScore = attackRoll + modifier + proficiency
        let armorClass = Character.armorClass target
        let hit = criticalHit || attackScore >= armorClass
        if hit then 
            let roll = if criticalHit then roll 2 else roll 1
            let damage = Math.Max(0, roll damageDice + modifier)
            let result = if criticalHit then CriticalHit else Hit
            result (attacker, [ target ], damage, damageType)
        else Miss (attacker, [ target ])
        

let castASpell (spell : Spell) (spellcaster : Character) (targets : Character list) =
    // let d20 = Dice 20
    // let proficiency = Character.proficiencyBonus spellcaster.Stats.Level
    // let saveThrows = 8 + spellcaster.AbilityModifiers.Intelligence + proficiency
    // let damageModifier = spellcaster.AbilityModifiers.Intelligence + proficiency
    spell.Effect spell.Data spellcaster targets
    



    
let character (combatant : Combatant) =
    combatant.Character

let isAnyAlive (combatants : Combatant seq) =
    Seq.exists (character >> Character.isAlive) combatants

let isFightOnGoing (state : CombatState) =
    state.TurnList
    |> Seq.groupBy (fun x -> x.Party.ID)
    |> Seq.map (snd >> isAnyAlive)
    |> Seq.forall id 

let selectTargetByHP (attacker : Combatant) (targets : Combatant list) =
    targets
    |> Seq.filter (fun x -> x.Party.ID <> attacker.Party.ID)
    |> Seq.filter (fun x -> Character.isAlive x.Character)
    |> Seq.sortBy (fun x -> x.Character.Stats.HP)
    |> Seq.tryHead
    
let runTurn (state : CombatState) =
    if isFightOnGoing state then
        match state.TurnList with
        | [] | [ _ ] -> state
        | attacker :: targets -> 
            let targets', result = 
                if Character.isAlive attacker.Character then
                    maybe {
                        let! target = selectTargetByHP attacker targets
                        let result = attack attacker.Character.Weapon attacker.Character target.Character
                        match result with
                        | Hit (_, _, damage, damageType) 
                        | CriticalHit (_, _, damage, damageType) ->
                            let character =  Character.appyDamage damage target.Character
                            let target' = { target with Character = character }
                            return targets
                                    |> List.map (fun x -> if x.Character.ID = target'.Character.ID then target'
                                                          else x)
                                    , result
                        | Miss (_, _) ->
                            return targets, result
                        | _ ->
                            return targets, result
                    }
                    |> Option.defaultValue (targets, NoTarget attacker.Character)
                else
                    targets, AttackerIsDead attacker.Character
            { state with TurnList = List.append targets' [ attacker ]
                         AttackResults = result :: state.AttackResults }
    else 
        state
