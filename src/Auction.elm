module Auction exposing (..)

import List exposing (drop, take)
import Participant exposing (Participant)


type alias Auction =
    { history : List HistoryItem
    , total : Float
    , active : List Participant
    , factor : Float
    }


type alias HistoryLogItem =
    { bidAmount : Maybe Float
    , bidder : Participant
    }


type HistoryItem
    = Bid Float Participant
    | Fold Participant


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


nextOffer : Auction -> Float
nextOffer currState =
    let
        factor =
            getNextFactor currState.factor (getFactorDirection currState)
    in
    currState.total * factor


getFactorDirection : Auction -> Bool
getFactorDirection auction =
    let
        auctionLen =
            List.length auction.active
    in
    case auctionLen of
        0 ->
            False

        1 ->
            False

        _ ->
            True


auctionIsDone : Auction -> Bool
auctionIsDone auction =
    let
        highest =
            getHighest (List.filter isBidItem auction.history)

        hasWinner =
            List.length auction.active == 1
    in
    if highest /= Nothing && hasWinner then
        True

    else
        False


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
    if dir then
        cur + offset

    else
        cur - offset


getHighest : List HistoryItem -> Maybe HistoryItem
getHighest auction =
    List.head auction


cycleActive : List Participant -> List Participant
cycleActive active =
    let
        newBack =
            take 1 active

        rest =
            drop 1 active
    in
    rest ++ newBack
