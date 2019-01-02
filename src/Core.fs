module FSharpDnD.Core

open System
open FSharpDnD.Prelude
open FSharpDnD.Types

module Equipments =
    module Armors =
        let padded = { Name = "Padded"; Class = 11; Type = ArmorTypes.Light; StealthDisadvantage = true }
        let leather = { Name = "Leather"; Class = 11; Type = ArmorTypes.Light; StealthDisadvantage = false }
        let studdedLeather = { Name = "Studded leather"; Class = 12; Type = ArmorTypes.Light; StealthDisadvantage = false }

        let hide = { Name = "Hide"; Class = 12; Type = ArmorTypes.Medium; StealthDisadvantage = false }
        let chainShirt = { Name = "Chain shirt"; Class = 13; Type = ArmorTypes.Medium; StealthDisadvantage = false }
        let scaleMail = { Name = "Scale mail"; Class = 14; Type = ArmorTypes.Medium; StealthDisadvantage = true }
        let breastplate = { Name = "Breastplate"; Class = 14; Type = ArmorTypes.Medium; StealthDisadvantage = false }
        let halfPlate = { Name = "Half plate"; Class = 15; Type = ArmorTypes.Medium; StealthDisadvantage = true }

        let ringMail = { Name = "Ring mail"; Class = 14; Type = ArmorTypes.Heavy; StealthDisadvantage = true }
        let chainMail = { Name = "Chain mail"; Class = 16; Type = ArmorTypes.Heavy; StealthDisadvantage = true }
        let splint = { Name = "Splint"; Class = 17; Type = ArmorTypes.Heavy; StealthDisadvantage = true }
        let plate = { Name = "Plate"; Class = 18; Type = ArmorTypes.Heavy; StealthDisadvantage = true }

        let shield = { Name = "Shield"; Class = 2; Type = ArmorTypes.Shield; StealthDisadvantage = false }



module AbilityScore =
    let empty = 
        { Strength = 0
          Dexterity = 0
          Constitution = 0
          Intelligence = 0
          Wisdom = 0
          Charisma = 0 }

module Armor =
    let armorClass (dexterityModifier : int) (armor : Armor) =
        let modifier = match armor.Type with
                        | ArmorTypes.Light -> dexterityModifier
                        | ArmorTypes.Medium -> Math.Min (2, dexterityModifier)
                        | ArmorTypes.Heavy 
                        | ArmorTypes.Shield -> 0
        armor.Class + modifier     

module Character =
    let empty =
        { ID = String.Empty
          Name = String.Empty
          Gender = Neuter
          Race = Races.Human
          Class = Classes.Cleric
          Abilities = AbilityScore.empty
          AbilityModifiers = AbilityScore.empty
          Weapon = None
          Armor = None
          Shield = None
          SpellSlots = []
          Stats =
              { Level = 1
                XP = 0
                HP = 0
                Age = 0 }
          ClassFeatures =
               { HitDice = Dice 0
                 HPFirst = 0
                 HPHigherLevels = 0
                 Proficiencies = 
                    { Armors = []
                      Weapons = []
                      Tools = []
                      SavingThrows = []
                      Skills = [] } } }


    let level (xp : int) =
        [ 300,      1
          900,      2
          2_700,    3
          6_500,    4
          14_000,   5
          23_000,   6
          34_000,   7
          48_000,   8
          64_000,   9
          85_000,   10
          100_000,  11
          120_000,  12
          140_000,  13
          165_000,  14
          195_000,  15
          225_000,  16
          265_000,  17
          305_000,  18
          355_000,  19 ]
        |> findInTable 20 ((<=) xp)

    let racialIncreases (race : Races) (abilities : AbilityScore) =
        let zero = AbilityScore.empty
        [ Dwarf,    { zero with Constitution = 2; Wisdom = 1 }
          Elf,      { zero with Dexterity = 2; Intelligence = 1; Wisdom = 1 }
          Halfling, { zero with Dexterity = 2; Constitution = 1; Charisma = 1 }
          Human,    { zero with Strength = 1; Dexterity = 1; Constitution = 1; Intelligence = 1; Wisdom = 1; Charisma = 1 } ]
        |> List.tryFind (fst >> (=) race)
        |> Option.map (fun (_, incr) ->
            { abilities with 
                Strength = abilities.Strength + incr.Strength
                Dexterity = abilities.Dexterity + incr.Dexterity
                Constitution = abilities.Constitution + incr.Constitution
                Intelligence = abilities.Intelligence + incr.Intelligence
                Wisdom = abilities.Wisdom + incr.Wisdom
                Charisma = abilities.Charisma + incr.Charisma }
        )
        |> Option.defaultValue abilities

    let abilityScoreModifier (score : int) =
        (float score - 10.0) / 2.0
        |> Math.Floor
        |> int
        // [ 1, -5
        //   3, -4
        //   5, -3
        //   7, -2
        //   9, -1
        //   11, 0
        //   13, 1
        //   15, 2
        //   17, 3
        //   19, 4
        //   21, 5
        //   23, 6
        //   25, 7
        //   27, 8
        //   29, 9
        //   30, 10 ]
        // |> findInTable 0 ((<=) score)

    let abilityModifiers (abilities : AbilityScore) =
        { AbilityScore.empty with 
            Strength = abilityScoreModifier abilities.Strength
            Dexterity = abilityScoreModifier abilities.Dexterity
            Constitution = abilityScoreModifier abilities.Constitution
            Intelligence = abilityScoreModifier abilities.Intelligence
            Wisdom = abilityScoreModifier abilities.Wisdom
            Charisma = abilityScoreModifier abilities.Charisma }

    let classFeatures (klass : Classes) =
        match klass with
        | Cleric -> 
            { HitDice = d8
              HPFirst = 9
              HPHigherLevels = 6
              Proficiencies = 
                { Armors = [ ArmorTypes.Light; ArmorTypes.Medium; ArmorTypes.Shield ]
                  Weapons = [ SimpleMelee; SimpleRanged ]
                  Tools = []
                  SavingThrows = [ Wisdom; Charisma ]
                  Skills = [] } }
        | Fighter -> 
            { HitDice = d10
              HPFirst = 10
              HPHigherLevels = 6
              Proficiencies = 
                { Armors = [ ArmorTypes.Heavy; ArmorTypes.Light; ArmorTypes.Medium; ArmorTypes.Shield ]
                  Weapons = [ SimpleMelee; SimpleRanged; MartialMelee; MartialRanged ]
                  Tools = []
                  SavingThrows = [ Strength; Constitution ]
                  Skills = [] } }
        | Rogue -> 
            { HitDice = d8
              HPFirst = 8
              HPHigherLevels = 5
              Proficiencies = 
                { Armors = [ ArmorTypes.Light ]
                  Weapons = [ SimpleMelee; SimpleRanged; MartialMelee; MartialRanged ]
                  Tools = []
                  SavingThrows = [ Dexterity; Intelligence ]
                  Skills = [] } }
        | Wizard -> 
            { HitDice = d6
              HPFirst = 6
              HPHigherLevels = 4
              Proficiencies = 
                { Armors = []
                  Weapons = [ SimpleMelee; SimpleRanged; ]
                  Tools = []
                  SavingThrows = [ Intelligence; Wisdom ]
                  Skills = [] } }

    let maximumHitPoints (classFeatures : ClassFeatures) (constitutionModifier : int) (level : int) =
        if level = 1 then 
            classFeatures.HPFirst + constitutionModifier
        else
            classFeatures.HPFirst + constitutionModifier + classFeatures.HPHigherLevels * (level - 1)

    let maximumHitPointsOf (character : Character) =
        maximumHitPoints character.ClassFeatures character.AbilityModifiers.Constitution character.Stats.Level

    let proficiencyBonus (level : int) =
        2 + ((level - 1) / 4)

    let gainXP (character : Character) (xp : int) =
        let xp' = xp + character.Stats.XP
        let lvl = level xp'
        let hp = maximumHitPoints 
                    character.ClassFeatures 
                    character.AbilityModifiers.Constitution 
                    lvl
        let stats = 
            { character.Stats with 
                Level = lvl
                XP = xp'
                HP = hp }
        { character with Stats = stats }            

    let isAlive (character : Character) =
        character.Stats.HP > 0      

    let isDead (character : Character) =
        character.Stats.HP <= 0

    let armorClass (character : Character) =
        let armor = match character.Armor with
                    | Some x -> Armor.armorClass character.AbilityModifiers.Dexterity x
                    | None -> 10
        let shield = match character.Shield with
                     | Some x -> Armor.armorClass character.AbilityModifiers.Dexterity x
                     | None -> 0
        armor + shield

    let difficultyClass (ability : Abilities) (character : Character) =
        let modifier = 
            match ability with
            | Strength -> character.AbilityModifiers.Strength
            | Dexterity -> character.AbilityModifiers.Dexterity
            | Constitution -> character.AbilityModifiers.Constitution
            | Intelligence -> character.AbilityModifiers.Intelligence
            | Wisdom -> character.AbilityModifiers.Wisdom
            | Charisma -> character.AbilityModifiers.Charisma
        let proficiencies = 
            if List.contains ability character.ClassFeatures.Proficiencies.SavingThrows then
                proficiencyBonus character.Stats.Level
            else 0
        8 + modifier + proficiencies

    let appyDamage (damage : int) (character : Character) =
        let stats = character.Stats
        let stats' = { stats with HP = Math.Max(0, stats.HP - damage) }
        { character with Stats = stats' }

    let generate (race : Races) 
                 (klass : Classes) 
                 (name : string) 
                 (gender : Genders)
                 (age : int) : Character =

        let classFeatures = classFeatures klass

        let roll () = 
            List.init 4 (fun _ -> rollOnce d6)
            |> List.sortDescending
            |> List.take 3
            |> List.sum

        let abilityScores = 
            { AbilityScore.empty with
                Strength = roll ()
                Dexterity = roll ()
                Constitution = roll ()
                Intelligence = roll ()
                Wisdom = roll ()
                Charisma = roll () }
            |> racialIncreases race

        let abilitiesModifiers = abilityModifiers abilityScores

        let stats = 
            { Level = 1
              XP = 0
              HP = maximumHitPoints classFeatures abilitiesModifiers.Constitution 1
              Age = age }

        { ID = Guid.NewGuid().ToString()
          Name = name
          Gender = gender
          Race = race
          Class = klass
          Abilities = abilityScores
          AbilityModifiers = abilitiesModifiers
          Stats = stats
          ClassFeatures = classFeatures 
          Weapon = None
          Armor = None
          Shield = None
          SpellSlots = [] }



