module Auction exposing (..)

import Participant exposing (Participant)


getNextFactor : Float -> Bool -> Float
getNextFactor cur dir =
    let
        offset =
            cur / 2
    in
    if cur == 1.0 then
        cur

    else if dir then
        cur + offset

    else
        cur - offset
