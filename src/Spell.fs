module FSharpDnD.Spell

open System
open Types
open FSharpDnD.Core


module Effects =

    let health (recover : int) (target : Character) =
        let max = Character.maximumHitPointsOf target
        let hp = Math.Min (max, target.Stats.HP + recover)
        let state = { target.Stats with HP = hp }
        { target with Stats = state }



module Cleric =

    let aid : Spell = 
        { ID = Guid.NewGuid ()
          Name = "Aid" 
          Data = 
            { Level = 2
              MagicSchools = MagicSchools.Abjuration
              CastingTime = 1
              Range = 1
              Components = [ SpellComponents.Verbal; SpellComponents.Material; SpellComponents.Somatic ]
              Duration = SpellDuration.Concentration
              Areas = SpellAreas.Cone }
          Effect = 
            fun x y targets -> 
                List.map (Effects.health 5) targets }
