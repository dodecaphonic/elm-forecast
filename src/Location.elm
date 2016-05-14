module Forecast.Location exposing (Location)


type alias Location = { name : String
                      , latitude : Float
                      , longitude : Float
                      , isSelected : Bool
                      , id : Int
                      }
