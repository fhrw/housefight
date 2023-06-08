module Room exposing (..)

import Participant exposing (Participant)


type alias Room =
    { title : String
    , status : RoomStatus
    }


type RoomStatus
    = Unallocated
    | Allocated Participant
