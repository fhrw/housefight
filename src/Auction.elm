module Auction exposing (..)

import Participant exposing (Participant)


type alias HistoryLogItem =
    { bidAmount : Maybe Float
    , bidder : Participant
    }


type HistoryItem
    = Bid Float Participant
    | Fold Participant


calcOverspray : Float -> Float -> Float
calcOverspray bid total =
    let
        remainder =
            bid - total
    in
    if remainder < 0 then
        0

    else
        remainder


hasWinner : List HistoryItem -> Bool
hasWinner hist =
    let
        bids =
            onlyBids hist

        topBid =
            List.head bids
    in
    case topBid of
        Nothing ->
            False

        _ ->
            True


onlyBids : List HistoryItem -> List HistoryItem
onlyBids hist =
    List.filter isBidItem hist


loggifyHistoryItem : HistoryItem -> HistoryLogItem
loggifyHistoryItem item =
    case item of
        Bid bid person ->
            { bidAmount = Just bid, bidder = person }

        Fold person ->
            { bidAmount = Nothing, bidder = person }


parseHistoryLogItem : HistoryLogItem -> String
parseHistoryLogItem item =
    case item.bidAmount of
        Nothing ->
            String.concat [ "{", item.bidder.name, " folded...", "}" ]

        Just amount ->
            String.concat [ "{", item.bidder.name, " made a bid of ", String.fromFloat amount, "}" ]


isBidItem : HistoryItem -> Bool
isBidItem item =
    case item of
        Bid _ _ ->
            True

        _ ->
            False


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


getHighest : List HistoryItem -> Maybe HistoryItem
getHighest auction =
    List.head auction
