module Forecast.Location(Location) where


type alias Location = { name : String
                      , latitude : Float
                      , longitude : Float
                      , isSelected : Bool
                      , id : Int
                      }
