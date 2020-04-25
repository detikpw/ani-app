module Playground exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Array exposing (Array)


suite : Test
suite =
    test "Test date function"
       (\_ -> Expect.equal "Des 12, 2019" (dateToString exampleDate))

dateToString : StartDate -> String
dateToString { year, month, day } =
    case (year, month, day) of
        (Just y, Just m, Just d) ->
            let
                m_ = Array.get (m - 1) months
                    
                d_ = String.fromInt d
                y_ = String.fromInt y
            in
            case m_ of
                Just monthInString ->
                    monthInString ++ " " ++ d_ ++ ", " ++ y_
            
                _ -> ""
    
        _ -> ""
    

exampleDate : StartDate
exampleDate =
    StartDate (Just 2019) (Just 12) (Just 12)


months : Array String
months =
    Array.fromList
        [ "Jan", "Feb", "Mar", "Apr", "Mei", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Des" ]

type alias StartDate =
    { year : Maybe Int
    , month : Maybe Int
    , day : Maybe Int
    }