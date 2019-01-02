module FSharpDnD.Types

open System
open Prelude

// https://github.com/Miserlou/dnd-tldr#character-sheets
// Ability              Meaning                 Modifies
// Strength (STR)       Physical strength       Moving things, hitting things, breaking doors, doing damage
// Dexterity (DEX)      Gymnastic ability       Generates armour class, climbing, dodging, balancing, archery
// Constitution (CON)   Health and stamina      Generates hit points, concentration
// Intelligence (INT)   Reasoning and learning  Deciphering, appraising, forgery, searching, spellcasting
// Wisdom (WIS)         Knowledge, gut feelings Knowing arcane knowledge, sensing traps and secrets, listening, healing
// Charisma (CHA)       Seductive ability       Seduction, charm, deception, disguise, animal taming

[<Measure>] type cp
[<Measure>] type sp
[<Measure>] type ep
[<Measure>] type gp
[<Measure>] type pp

type CharacterID = string    

and Character = 
    { ID : CharacterID
      Name : string
      Gender : Genders
      Race : Races
      Class : Classes
      Abilities : AbilityScore
      AbilityModifiers : AbilityScore
      Stats : CharacterStats
      ClassFeatures : ClassFeatures
      Weapon : Weapon option
      Armor : Armor option
      Shield : Armor option
      SpellSlots : Spell list }
and Genders = 
    | Male
    | Female       
    | Neuter   
and Races = 
    | Dwarf
    | Elf
    | Halfling
    | Human
and Classes =
    | Cleric
    | Fighter
    | Rogue
    | Wizard
and ClassFeatures = 
    { HitDice : Dice
      HPFirst : int
      HPHigherLevels : int
      Proficiencies : Proficiencies }
and Abilities = 
    | Strength
    | Dexterity
    | Constitution
    | Intelligence
    | Wisdom
    | Charisma
and AbilityScore = 
    { Strength : int
      Dexterity : int
      Constitution : int
      Intelligence : int
      Wisdom : int
      Charisma : int }
and CharacterStats = 
    { Level : int
      XP : int
      HP : int
      Age : int }
and Alighments = 
    | LawfulGood
    | LawfulNeutral
    | LawfulEvil
    | NeutralGood
    | TrueNeutral
    | NeutralEvil
    | ChaoticGood
    | ChaoticNeutral
    | ChaoticEvil
and ArmorTypes =
    | Light
    | Medium
    | Heavy
    | Shield
and WeaponTypes =
    | SimpleMelee
    | SimpleRanged   
    | MartialMelee
    | MartialRanged
and Proficiencies = 
    { Armors : ArmorTypes list
      Weapons : WeaponTypes list 
      Tools : obj list
      SavingThrows : Abilities list
      Skills : obj list }
and Weapon =
    { Name : string
      DamageDice : Dice
      DamageType : DamageTypes
      Type : WeaponTypes
      Properties : WeaponProperty list }
and WeaponProperty =
    | Ammunition // 弹药，消耗品
    | Finesse
    | Heavy
    | Light
    | Loading of time:int // 装填，需要时间
    | Range of min:int * max:int
    | Special
    | TowHanded // 双手武器
    | Versatile // 两用，可单可双
and DamageTypes =
    | Nil
    | Acid
    | Bludgeoning
    | Cold
    | Fire
    | Force
    | Lightning
    | Necrotic
    | Piercing
    | Poison
    | Psychic
    | Radiant
    | Slashing
    | Thunder
and Armor = 
    { Name : string
      Class : int
      Type : ArmorTypes
      StealthDisadvantage : bool }
and Spell = 
    { ID : Guid
      Name : string
      Data : SpellData
      Effect : SpellEffect }
and SpellData =
    { Level : int
      MagicSchools : MagicSchools
      CastingTime : int
      Range : int
      Components : SpellComponents list
      Duration : SpellDuration
      Areas : SpellAreas }
and MagicSchools =
    | Abjuration
    | Conjuration
    | Divination
    | Enchantment
    | Evocation
    | Illusion
    | Necromancy
    | Transmutation
and SpellComponents =
    | Verbal
    | Somatic
    | Material      
and SpellDuration =
    | Instantaneous
    | Concentration
and SpellAreas =
    | Point
    | Cone
    | Cube
    | Cylinder
    | Line
    | Sphere
and SpellEffect = SpellData -> Character -> Character list -> Character list


and CombatActions =
    | Attack of weapon:Weapon * attacker:Character * target:Character
    | CastASpell of spell:Spell * spellcaster:Character * targets:Character list
    | Dash
    | Help
    | UseAnObject
and AttackResults =
    | Hit of attacker:Character * target:Character list * damage:int * damageType:DamageTypes
    | CriticalHit of attacker:Character * target:Character list * damage:int * damageType:DamageTypes
    | Miss of attacker:Character * target:Character list
    | NoTarget of attacker:Character
    | AttackerIsDead of attacker:Character
and Party = 
    { ID : string
      Members : Character list }



type AreaID = string

and Area = 
    { ID : AreaID
      Name : string
      Description : string
      Portals : AreaID list }

