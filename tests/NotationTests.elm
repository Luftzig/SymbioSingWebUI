module NotationTests exposing (..)

import Array
import Composer.Notation as Notation exposing (Dynamic(..), HapticNote(..), HapticScore, Measure, Signature, parseMusicXml)
import Dict
import Expect
import Extra.TypedTime as TypedTime
import FlowIO exposing (defaultCommand)
import List.Extra
import Scheduler
import Test exposing (..)


convertHapticScoreSuite : Test
convertHapticScoreSuite =
    let
        signature4on4 : Signature
        signature4on4 =
            { beats = 4, beatType = 4 }

        baseMeasure : Measure
        baseMeasure =
            { signature = signature4on4
            , divisionsPerQuarter = 2
            , notes = []
            , number = 1
            }

        portsAllClosed =
            defaultCommand.ports

        commandStop =
            { defaultCommand | action = FlowIO.Stop }

        {--Given in jumps of (255/7)
        --}
        defaultDynamics =
            { pianissimo = 36
            , piano = 73
            , mezzopiano = 109
            , mezzoforte = 146
            , forte = 182
            , fortissimo = 219
            , fortississimo = 255
            }
    in
    describe "convert haptic score to schedule"
        [ describe "convert single part scores"
            [ test "empty part" <|
                \() ->
                    let
                        score : HapticScore
                        score =
                            Dict.fromList
                                [ ( "P1"
                                  , { name = "test-name"
                                    , measures =
                                        []
                                    }
                                  )
                                ]

                        emptyInstructions : Scheduler.Instructions
                        emptyInstructions =
                            { time = Array.fromList [ TypedTime.milliseconds 0 ]
                            , instructions =
                                Dict.fromList
                                    [ ( "role-1", Array.fromList [ commandStop ] ) ]
                            }
                    in
                    Notation.scoreToSchedule
                        { bpm = 100
                        , roleMapping = Dict.fromList [ ( "P1", ( "role-1", FlowIO.Port1 ) ) ]
                        , dynamics = defaultDynamics
                        }
                        score
                        |> Expect.equal (Ok emptyInstructions)
            , test "one measure, one rest" <|
                \() ->
                    let
                        score : HapticScore
                        score =
                            Dict.fromList
                                [ ( "P1"
                                  , { name = "test-name"
                                    , measures =
                                        [ { baseMeasure
                                            | notes =
                                                [ Hold Pianississimo (baseMeasure.divisionsPerQuarter * 4) ]
                                          }
                                        ]
                                    }
                                  )
                                ]

                        expectedInstructions : Scheduler.Instructions
                        expectedInstructions =
                            { time = Array.fromList [ TypedTime.milliseconds 0, TypedTime.seconds 4 ]
                            , instructions =
                                Dict.fromList
                                    [ ( "role-1"
                                      , Array.fromList
                                            [ commandStop
                                            , commandStop
                                            ]
                                      )
                                    ]
                            }
                    in
                    Notation.scoreToSchedule
                        { bpm = 60
                        , roleMapping = Dict.fromList [ ( "P1", ( "role-1", FlowIO.Port1 ) ) ]
                        , dynamics = defaultDynamics
                        }
                        score
                        |> Expect.equal (Ok expectedInstructions)
            , test "one measure, note and rest" <|
                \() ->
                    let
                        score : HapticScore
                        score =
                            Dict.fromList
                                [ ( "P1"
                                  , { name = "test-name"
                                    , measures =
                                        [ { baseMeasure
                                            | notes =
                                                [ Hold Mezzopiano (baseMeasure.divisionsPerQuarter * 2)
                                                , Actuate Fortississimo (baseMeasure.divisionsPerQuarter * 2)
                                                ]
                                          }
                                        ]
                                    }
                                  )
                                ]

                        expectedInstructions : Scheduler.Instructions
                        expectedInstructions =
                            { time = Array.fromList [ TypedTime.milliseconds 0, TypedTime.seconds 1, TypedTime.seconds 2 ]
                            , instructions =
                                Dict.fromList
                                    [ ( "role-1"
                                      , Array.fromList
                                            [ { commandStop | pumpPwm = 0 }
                                            , { action = FlowIO.Inflate
                                              , pumpPwm = defaultDynamics.fortississimo
                                              , ports =
                                                    { portsAllClosed | port1 = FlowIO.PortOpen }
                                              }
                                            , commandStop
                                            ]
                                      )
                                    ]
                            }
                    in
                    Notation.scoreToSchedule
                        { bpm = 120
                        , roleMapping = Dict.fromList [ ( "P1", ( "role-1", FlowIO.Port1 ) ) ]
                        , dynamics = defaultDynamics
                        }
                        score
                        |> Expect.equal (Ok expectedInstructions)
            , test "one measure, inflate, release" <|
                \() ->
                    let
                        score : HapticScore
                        score =
                            Dict.fromList
                                [ ( "P2"
                                  , { name = "test-name"
                                    , measures =
                                        [ { baseMeasure
                                            | notes =
                                                [ Actuate Fortississimo (baseMeasure.divisionsPerQuarter * 1)
                                                , Rest Mezzoforte (baseMeasure.divisionsPerQuarter * 3)
                                                ]
                                          }
                                        ]
                                    }
                                  )
                                ]

                        expectedInstructions : Scheduler.Instructions
                        expectedInstructions =
                            { time = Array.fromList [ TypedTime.milliseconds 0, TypedTime.seconds 0.5, TypedTime.seconds 2 ]
                            , instructions =
                                Dict.fromList
                                    [ ( "role-2"
                                      , Array.fromList
                                            [ { action = FlowIO.Inflate
                                              , pumpPwm = defaultDynamics.fortississimo
                                              , ports =
                                                    { portsAllClosed | port1 = FlowIO.PortOpen }
                                              }
                                            , { action = FlowIO.Release
                                              , pumpPwm = 0
                                              , ports =
                                                    { portsAllClosed | port1 = FlowIO.PortOpen }
                                              }
                                            , commandStop
                                            ]
                                      )
                                    ]
                            }
                    in
                    Notation.scoreToSchedule
                        { bpm = 120
                        , roleMapping = Dict.fromList [ ( "P2", ( "role-2", FlowIO.Port1 ) ) ]
                        , dynamics = defaultDynamics
                        }
                        score
                        |> Expect.equal (Ok expectedInstructions)
            , test "port closing is correct" <|
                \_ ->
                    let
                        score : HapticScore
                        score =
                            Dict.fromList
                                [ ( "P2"
                                  , { name = "test-name"
                                    , measures =
                                        [ { baseMeasure
                                            | notes =
                                                [ Actuate Fortississimo (baseMeasure.divisionsPerQuarter * 1)
                                                , Hold Fortissimo (baseMeasure.divisionsPerQuarter * 1)
                                                , Rest Mezzoforte (baseMeasure.divisionsPerQuarter * 2)
                                                ]
                                          }
                                        ]
                                    }
                                  )
                                ]

                        expectedInstructions : Scheduler.Instructions
                        expectedInstructions =
                            { time =
                                Array.fromList
                                    [ TypedTime.milliseconds 0
                                    , TypedTime.seconds 0.5
                                    , TypedTime.seconds 1
                                    , TypedTime.seconds 2
                                    ]
                            , instructions =
                                Dict.fromList
                                    [ ( "role-2"
                                      , Array.fromList
                                            [ { action = FlowIO.Inflate
                                              , pumpPwm = defaultDynamics.fortississimo
                                              , ports =
                                                    { portsAllClosed | port1 = FlowIO.PortOpen }
                                              }
                                            , { action = FlowIO.Stop
                                              , pumpPwm = 0
                                              , ports =
                                                    { portsAllClosed | port1 = FlowIO.PortClosed }
                                              }
                                            , { action = FlowIO.Release
                                              , pumpPwm = 0
                                              , ports =
                                                    { portsAllClosed | port1 = FlowIO.PortOpen }
                                              }
                                            , commandStop
                                            ]
                                      )
                                    ]
                            }
                    in
                    Notation.scoreToSchedule
                        { bpm = 120
                        , roleMapping = Dict.fromList [ ( "P2", ( "role-2", FlowIO.Port1 ) ) ]
                        , dynamics = defaultDynamics
                        }
                        score
                        |> Expect.equal (Ok expectedInstructions)
            , test "trill" <|
                \_ ->
                    let
                        score : HapticScore
                        score =
                            Dict.fromList
                                [ ( "P1"
                                  , { name = "test"
                                    , measures =
                                        [ { baseMeasure
                                            | notes = [ Trill Mezzoforte (baseMeasure.divisionsPerQuarter * 4) ]
                                          }
                                        ]
                                    }
                                  )
                                ]

                        completeMeasureTime =
                            TypedTime.milliseconds 2000

                        inflateInstructions =
                            { action = FlowIO.Inflate
                            , pumpPwm = defaultDynamics.mezzoforte
                            , ports =
                                { portsAllClosed | port1 = FlowIO.PortOpen }
                            }

                        holdInstruction =
                            { action = FlowIO.Stop
                            , pumpPwm = 0
                            , ports =
                                { portsAllClosed | port1 = FlowIO.PortClosed }
                            }

                        divisionsInMeasure =
                            baseMeasure.divisionsPerQuarter
                                * baseMeasure.signature.beats

                        expected : Scheduler.Instructions
                        expected =
                            { time =
                                Array.fromList
                                    (List.range 0 divisionsInMeasure
                                        |> List.map
                                            (\i ->
                                                (completeMeasureTime |> TypedTime.divide (toFloat divisionsInMeasure))
                                                    |> TypedTime.multiply (toFloat i)
                                            )
                                    )
                            , instructions =
                                Dict.fromList
                                    [ ( "role-1"
                                      , Array.fromList
                                            ((List.range 0 (divisionsInMeasure - 1)
                                                |> List.map
                                                    (\i ->
                                                        if (i |> modBy 2) == 0 then
                                                            inflateInstructions

                                                        else
                                                            holdInstruction
                                                    )
                                             )
                                                ++ [ holdInstruction ]
                                            )
                                      )
                                    ]
                            }
                    in
                    Notation.scoreToSchedule
                        { bpm = 120
                        , roleMapping = Dict.fromList [ ( "P1", ( "role-1", FlowIO.Port1 ) ) ]
                        , dynamics = defaultDynamics
                        }
                        score
                        |> Expect.equal (Ok expected)
            ]
        , describe "two parts"
            [ test "empty parts" <|
                \() ->
                    let
                        score : HapticScore
                        score =
                            Dict.fromList
                                [ ( "P1"
                                  , { name = "test-name"
                                    , measures =
                                        []
                                    }
                                  )
                                , ( "P2"
                                  , { name = "test part 2"
                                    , measures = []
                                    }
                                  )
                                ]

                        emptyInstructions : Scheduler.Instructions
                        emptyInstructions =
                            { time = Array.fromList [ TypedTime.milliseconds 0 ]
                            , instructions =
                                Dict.fromList
                                    [ ( "role-1", Array.fromList [ commandStop ] )
                                    ]
                            }
                    in
                    Notation.scoreToSchedule
                        { bpm = 100
                        , roleMapping =
                            Dict.fromList
                                [ ( "P1", ( "role-1", FlowIO.Port1 ) )
                                , ( "P2", ( "role-1", FlowIO.Port2 ) )
                                ]
                        , dynamics = defaultDynamics
                        }
                        score
                        |> Expect.equal (Ok emptyInstructions)
            , test "two synchronous parts" <|
                \() ->
                    let
                        score : HapticScore
                        score =
                            Dict.fromList
                                [ ( "P1"
                                  , { name = "part 1"
                                    , measures =
                                        [ { baseMeasure
                                            | notes =
                                                [ Actuate Fortississimo (baseMeasure.divisionsPerQuarter * 1)
                                                , Rest Mezzoforte (baseMeasure.divisionsPerQuarter * 3)
                                                ]
                                          }
                                        ]
                                    }
                                  )
                                , ( "P2"
                                  , { name = "part 2"
                                    , measures =
                                        [ { baseMeasure
                                            | notes =
                                                [ Actuate Fortississimo (baseMeasure.divisionsPerQuarter * 1)
                                                , Rest Mezzoforte (baseMeasure.divisionsPerQuarter * 3)
                                                ]
                                          }
                                        ]
                                    }
                                  )
                                ]

                        expectedInstructions : Scheduler.Instructions
                        expectedInstructions =
                            { time = Array.fromList [ TypedTime.milliseconds 0, TypedTime.seconds 0.5, TypedTime.seconds 2 ]
                            , instructions =
                                Dict.fromList
                                    [ ( "role-1"
                                      , Array.fromList
                                            [ { action = FlowIO.Inflate
                                              , pumpPwm = defaultDynamics.fortississimo
                                              , ports =
                                                    { portsAllClosed | port1 = FlowIO.PortOpen, port2 = FlowIO.PortOpen }
                                              }
                                            , { action = FlowIO.Release
                                              , pumpPwm = 0
                                              , ports =
                                                    { portsAllClosed | port1 = FlowIO.PortOpen, port2 = FlowIO.PortOpen }
                                              }
                                            , commandStop
                                            ]
                                      )
                                    ]
                            }
                    in
                    Notation.scoreToSchedule
                        { bpm = 120
                        , roleMapping =
                            Dict.fromList
                                [ ( "P1", ( "role-1", FlowIO.Port1 ) )
                                , ( "P2", ( "role-1", FlowIO.Port2 ) )
                                ]
                        , dynamics = defaultDynamics
                        }
                        score
                        |> Expect.equal (Ok expectedInstructions)
            , test "one part inflates other part hold" <|
                \() ->
                    let
                        score : HapticScore
                        score =
                            Dict.fromList
                                [ ( "P1"
                                  , { name = "part 1"
                                    , measures =
                                        [ { baseMeasure
                                            | notes =
                                                [ Actuate Fortississimo (baseMeasure.divisionsPerQuarter * 1)
                                                , Actuate Mezzoforte (baseMeasure.divisionsPerQuarter * 3)
                                                ]
                                          }
                                        ]
                                    }
                                  )
                                , ( "P2"
                                  , { name = "part 2"
                                    , measures =
                                        [ { baseMeasure
                                            | notes =
                                                [ Hold Fortississimo (baseMeasure.divisionsPerQuarter * 4)
                                                ]
                                          }
                                        ]
                                    }
                                  )
                                ]

                        expectedInstructions : Scheduler.Instructions
                        expectedInstructions =
                            { time = Array.fromList [ TypedTime.milliseconds 0, TypedTime.seconds 0.5, TypedTime.seconds 2 ]
                            , instructions =
                                Dict.fromList
                                    [ ( "role-1"
                                      , Array.fromList
                                            [ { action = FlowIO.Inflate
                                              , pumpPwm = defaultDynamics.fortississimo
                                              , ports =
                                                    { portsAllClosed | port1 = FlowIO.PortOpen }
                                              }
                                            , { action = FlowIO.Inflate
                                              , pumpPwm = defaultDynamics.mezzoforte
                                              , ports =
                                                    { portsAllClosed | port1 = FlowIO.PortOpen }
                                              }
                                            , commandStop
                                            ]
                                      )
                                    ]
                            }
                    in
                    Notation.scoreToSchedule
                        { bpm = 120
                        , roleMapping =
                            Dict.fromList
                                [ ( "P1", ( "role-1", FlowIO.Port1 ) )
                                , ( "P2", ( "role-1", FlowIO.Port2 ) )
                                ]
                        , dynamics = defaultDynamics
                        }
                        score
                        |> Expect.equal (Ok expectedInstructions)
            , test "complex two parts" <|
                \() ->
                    let
                        score : HapticScore
                        score =
                            {- p1 = inflate - hold - inflate - release - hold
                               p2 = hold - inflate - hold - hold - release
                            -}
                            Dict.fromList
                                [ ( "P1"
                                  , { name = "part 1"
                                    , measures =
                                        [ { baseMeasure
                                            | notes =
                                                [ Actuate Fortississimo (baseMeasure.divisionsPerQuarter * 1)
                                                , Hold Mezzoforte (baseMeasure.divisionsPerQuarter * 1)
                                                , Actuate Mezzoforte (baseMeasure.divisionsPerQuarter * 1)
                                                , Rest Forte (baseMeasure.divisionsPerQuarter * 1)
                                                ]
                                          }
                                        , { baseMeasure
                                            | notes =
                                                [ Hold Forte (baseMeasure.divisionsPerQuarter * 1)
                                                ]
                                            , number = 2
                                          }
                                        ]
                                    }
                                  )
                                , ( "P2"
                                  , { name = "part 2"
                                    , measures =
                                        [ { baseMeasure
                                            | notes =
                                                [ Hold Fortississimo (baseMeasure.divisionsPerQuarter * 1)
                                                , Actuate Mezzoforte (baseMeasure.divisionsPerQuarter * 1)
                                                , Hold Mezzoforte (baseMeasure.divisionsPerQuarter * 2)
                                                ]
                                          }
                                        , { baseMeasure
                                            | notes =
                                                [ Rest Forte (baseMeasure.divisionsPerQuarter * 1)
                                                ]
                                            , number = 2
                                          }
                                        ]
                                    }
                                  )
                                ]

                        expectedInstructions : Scheduler.Instructions
                        expectedInstructions =
                            { time =
                                Array.fromList
                                    [ TypedTime.milliseconds 0
                                    , TypedTime.seconds 0.5
                                    , TypedTime.seconds 1
                                    , TypedTime.seconds 1.5
                                    , TypedTime.seconds 2
                                    , TypedTime.seconds 2.5
                                    ]
                            , instructions =
                                Dict.fromList
                                    [ ( "role-1"
                                        {- p1 = inflate fff - hold mf - inflate mf - release f - hold f
                                           p2 = hold fff - inflate mf - hold mf - hold mf - release f
                                        -}
                                      , Array.fromList
                                            [ { action = FlowIO.Inflate
                                              , pumpPwm = defaultDynamics.fortississimo
                                              , ports =
                                                    { portsAllClosed | port1 = FlowIO.PortOpen }
                                              }
                                            , { action = FlowIO.Inflate
                                              , pumpPwm = defaultDynamics.mezzoforte
                                              , ports =
                                                    { portsAllClosed | port2 = FlowIO.PortOpen }
                                              }
                                            , { action = FlowIO.Inflate
                                              , pumpPwm = defaultDynamics.mezzoforte
                                              , ports =
                                                    { portsAllClosed | port1 = FlowIO.PortOpen }
                                              }
                                            , { action = FlowIO.Release
                                              , pumpPwm = 0
                                              , ports =
                                                    { portsAllClosed | port1 = FlowIO.PortOpen }
                                              }
                                            , { action = FlowIO.Release
                                              , pumpPwm = 0
                                              , ports =
                                                    { portsAllClosed | port2 = FlowIO.PortOpen }
                                              }
                                            , commandStop
                                            ]
                                      )
                                    ]
                            }
                    in
                    Notation.scoreToSchedule
                        { bpm = 120
                        , roleMapping =
                            Dict.fromList
                                [ ( "P1", ( "role-1", FlowIO.Port1 ) )
                                , ( "P2", ( "role-1", FlowIO.Port2 ) )
                                ]
                        , dynamics = defaultDynamics
                        }
                        score
                        |> Expect.equal (Ok expectedInstructions)
            , test "trill" <|
                \_ ->
                    let
                        score : HapticScore
                        score =
                            Dict.fromList
                                [ ( "P1"
                                  , { name = "trill"
                                    , measures =
                                        [ { baseMeasure
                                            | notes = [ Trill Mezzoforte (baseMeasure.divisionsPerQuarter * 4) ]
                                          }
                                        ]
                                    }
                                  )
                                , ( "P2"
                                  , { name = "other instrument"
                                    , measures =
                                        [ { baseMeasure
                                            | notes =
                                                [ Actuate Fortissimo (baseMeasure.divisionsPerQuarter * 2)
                                                , Hold Fortissimo (baseMeasure.divisionsPerQuarter * 2)
                                                ]
                                          }
                                        ]
                                    }
                                  )
                                ]

                        completeMeasureTime =
                            TypedTime.milliseconds 2000

                        inflateInstructions =
                            { action = FlowIO.Inflate
                            , pumpPwm = defaultDynamics.mezzoforte
                            , ports =
                                { portsAllClosed | port1 = FlowIO.PortOpen }
                            }

                        holdInstruction =
                            { action = FlowIO.Stop
                            , pumpPwm = 0
                            , ports =
                                { portsAllClosed | port1 = FlowIO.PortClosed }
                            }

                        divisionsInMeasure =
                            baseMeasure.divisionsPerQuarter
                                * baseMeasure.signature.beats

                        expected : Scheduler.Instructions
                        expected =
                            { time =
                                Array.fromList
                                    (List.range 0 divisionsInMeasure
                                        |> List.map
                                            (\i ->
                                                (completeMeasureTime |> TypedTime.divide (toFloat divisionsInMeasure))
                                                    |> TypedTime.multiply (toFloat i)
                                            )
                                    )
                            , instructions =
                                Dict.fromList
                                    [ ( "role-1"
                                      , Array.fromList
                                            ((List.range 0 ((divisionsInMeasure // 2) - 1)
                                                |> List.map
                                                    (\i ->
                                                        if (i |> modBy 2) == 0 then
                                                            { action = FlowIO.Inflate
                                                            , pumpPwm = defaultDynamics.fortissimo
                                                            , ports =
                                                                { portsAllClosed
                                                                    | port1 = FlowIO.PortOpen
                                                                    , port2 = FlowIO.PortOpen
                                                                }
                                                            }

                                                        else
                                                            { action = FlowIO.Inflate
                                                            , pumpPwm = defaultDynamics.fortissimo
                                                            , ports =
                                                                { portsAllClosed
                                                                    | port1 = FlowIO.PortClosed
                                                                    , port2 = FlowIO.PortOpen
                                                                }
                                                            }
                                                    )
                                             )
                                                ++ (List.range 0 ((divisionsInMeasure // 2) - 1)
                                                        |> List.map
                                                            (\i ->
                                                                if (i |> modBy 2) == 0 then
                                                                    { action = FlowIO.Inflate
                                                                    , pumpPwm = defaultDynamics.mezzoforte
                                                                    , ports =
                                                                        { portsAllClosed
                                                                            | port1 = FlowIO.PortOpen
                                                                            , port2 = FlowIO.PortClosed
                                                                        }
                                                                    }

                                                                else
                                                                    { action = FlowIO.Stop
                                                                    , pumpPwm = 0
                                                                    , ports = portsAllClosed
                                                                    }
                                                            )
                                                   )
                                                ++ [ holdInstruction ]
                                            )
                                      )
                                    ]
                            }
                    in
                    Notation.scoreToSchedule
                        { bpm = 120
                        , roleMapping =
                            Dict.fromList
                                [ ( "P1", ( "role-1", FlowIO.Port1 ) ), ( "P2", ( "role-1", FlowIO.Port2 ) ) ]
                        , dynamics = defaultDynamics
                        }
                        score
                        |> Expect.equal (Ok expected)
            ]
        ]


parseMusicXmlSuite : Test
parseMusicXmlSuite =
    let
        ifParsedContentOk : String -> (Notation.HapticScore -> Expect.Expectation) -> Expect.Expectation
        ifParsedContentOk content do =
            case parseMusicXml content of
                Err e ->
                    Expect.fail ("parsing failed with error \"" ++ e ++ "\"")

                Ok score ->
                    do score
    in
    describe "test the parsing of a musicXML file"
        [ test "parsing succeeds" <|
            \_ ->
                parseMusicXml testContentV3
                    |> Expect.ok
        , test "parsing extracts parts with correct ids" <|
            \_ ->
                ifParsedContentOk testContentV3 <|
                    \parts ->
                        Dict.keys parts
                            |> List.sort
                            |> Expect.equalLists
                                (List.sort
                                    [ "P1"
                                    , "P2"
                                    , "P3"
                                    , "P4"
                                    , "P5"
                                    , "P6"
                                    , "P7"
                                    , "P8"
                                    , "P9"
                                    , "P10"
                                    , "P11"
                                    , "P12"
                                    , "P13"
                                    , "P14"
                                    , "P15"
                                    ]
                                )
        , test "parsing extracts parts with correct names" <|
            \_ ->
                ifParsedContentOk testContentV3 <|
                    \parts ->
                        Dict.values parts
                            |> List.map .name
                            |> Expect.equalLists
                                [ "Abs1"
                                , "SPARE"
                                , "RRib1"
                                , "RRib2"
                                , "RRib3"
                                , "Exhaust1"
                                , "Exhaust2"
                                , "Abs2"
                                , "LeftBack"
                                , "RightBack"
                                , "Spine"
                                , "LRib1"
                                , "LRib2"
                                , "LRib3"
                                , "Palm"
                                ]
        , describe "time signature"
            [ test "read time signature for first measure" <|
                \_ ->
                    ifParsedContentOk testSinglePart <|
                        \parts ->
                            parts
                                |> Dict.get "P1"
                                |> Maybe.map .measures
                                |> Maybe.andThen List.head
                                |> Maybe.map .signature
                                |> Expect.equal (Just { beats = 3, beatType = 8 })
            , test "read time division for first measure" <|
                \_ ->
                    ifParsedContentOk testSinglePart <|
                        \parts ->
                            parts
                                |> Dict.get "P1"
                                |> Maybe.map .measures
                                |> Maybe.andThen List.head
                                |> Maybe.map .divisionsPerQuarter
                                |> Expect.equal (Just 16)
            , test "read time signature for second measure which is copied from 1st" <|
                \_ ->
                    ifParsedContentOk testSinglePart <|
                        \parts ->
                            parts
                                |> Dict.get "P1"
                                |> Maybe.map .measures
                                |> Maybe.andThen (List.Extra.getAt 1)
                                |> Maybe.map .signature
                                |> Expect.equal (Just { beats = 3, beatType = 8 })
            , test "read time division for second measure which is copied from 1st" <|
                \_ ->
                    ifParsedContentOk testSinglePart <|
                        \parts ->
                            parts
                                |> Dict.get "P1"
                                |> Maybe.map .measures
                                |> Maybe.andThen (List.Extra.getAt 1)
                                |> Maybe.map .divisionsPerQuarter
                                |> Expect.equal (Just 16)
            ]
        , test "check that measure numbering is consistent" <|
            \_ ->
                ifParsedContentOk testSinglePart <|
                    \score ->
                        let
                            measures =
                                score
                                    |> Dict.get "P1"
                                    |> Maybe.map .measures
                                    |> Maybe.withDefault []

                            measuresCount =
                                measures
                                    |> List.length
                        in
                        measures
                            |> List.map .number
                            |> Expect.equalLists (List.range 1 measuresCount)

        -- TODO: Create a sample file to test this
        --, test "check that time signature change is registered" <|
        --    \_ ->
        --        ifParsedContentOk <|
        --            \score ->
        --                let
        --                    measure5 : Maybe Measure
        --                    measure5 =
        --                        score
        --                            |> Dict.get "P1"
        --                            |> Maybe.map .measures
        --                            |> Maybe.andThen (List.Extra.getAt 5)
        --                in
        --                measure5
        --                    |> Maybe.map .signature
        --                    |> Expect.equal (Just { beats = 3, beatType = 8 })
        , describe "parsing notes"
            [ test "parse first note" <|
                \_ ->
                    ifParsedContentOk testSinglePart <|
                        \score ->
                            score
                                |> Dict.get "P1"
                                |> Maybe.map .measures
                                |> Maybe.andThen (List.Extra.getAt 0)
                                |> Maybe.map .notes
                                |> Maybe.andThen List.head
                                |> Expect.equal (Just (Actuate Piano 14))
            , test "parse second note (rest)" <|
                \_ ->
                    ifParsedContentOk testSinglePart <|
                        \score ->
                            score
                                |> Dict.get "P1"
                                |> Maybe.map .measures
                                |> Maybe.andThen (List.Extra.getAt 0)
                                |> Maybe.map .notes
                                |> Maybe.andThen (List.Extra.getAt 1)
                                |> Expect.equal (Just (Rest Piano 8))
            , test "parse hold note" <|
                \_ ->
                    ifParsedContentOk testSinglePart <|
                        \score ->
                            score
                                |> Dict.get "P1"
                                |> Maybe.map .measures
                                |> Maybe.andThen (List.Extra.getAt 0)
                                |> Maybe.map .notes
                                |> Maybe.andThen (List.Extra.getAt 2)
                                |> Expect.equal (Just (Hold Pianississimo 2))
            ]
        ]


testSinglePart =
    """
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE score-partwise PUBLIC "-//Recordare//DTD MusicXML 3.1 Partwise//EN" "http://www.musicxml.org/dtds/partwise.dtd">
<score-partwise version="3.1">
  <part-list>
    <score-part id="P1">
      <part-name>Abs1</part-name>
      <part-abbreviation>A1</part-abbreviation>
    </score-part>
  </part-list>
  <part id="P1">
    <measure number="1" width="266.54">
      <attributes>
        <divisions>16</divisions>
        <key>
          <fifths>0</fifths>
          </key>
        <time>
          <beats>3</beats>
          <beat-type>8</beat-type>
          </time>
        <clef>
          <sign>percussion</sign>
          <line>2</line>
          </clef>
        <staff-details>
          <staff-lines>1</staff-lines>
          </staff-details>
        </attributes>
      <direction placement="below">
        <direction-type>
          <dynamics default-x="3.25" default-y="-26.84" relative-y="-25.00">
            <p/>
            </dynamics>
          </direction-type>
        <sound dynamics="54.44"/>
        </direction>
      <note default-x="69.00" default-y="0.00">
        <unpitched>
          <display-step>E</display-step>
          <display-octave>4</display-octave>
          </unpitched>
        <duration>14</duration>
        <instrument id="P1-I82"/>
        <voice>1</voice>
        <type>eighth</type>
        <dot/>
        <dot/>
        <stem>down</stem>
        </note>
      <note>
        <rest/>
        <duration>8</duration>
        <voice>1</voice>
        <type>eighth</type>
        </note>
      <note default-x="69.00" default-y="-423.62">
        <unpitched>
          <display-step>E</display-step>
          <display-octave>4</display-octave>
          </unpitched>
        <duration>2</duration>
        <instrument id="P6-I81"/>
        <voice>1</voice>
        <type>32nd</type>
        <stem>down</stem>
        <notehead>x</notehead>
        <beam number="1">begin</beam>
        <beam number="2">begin</beam>
        <beam number="3">forward hook</beam>
        </note>
    </measure>
    <measure number="2" width="266.54">
      <note default-x="69.00" default-y="0.00">
        <unpitched>
          <display-step>E</display-step>
          <display-octave>4</display-octave>
          </unpitched>
        <duration>14</duration>
        <instrument id="P1-I82"/>
        <voice>1</voice>
        <type>eighth</type>
        <dot/>
        <dot/>
        <stem>down</stem>
        </note>
      <note>
        <rest/>
        <duration>8</duration>
        <voice>1</voice>
        <type>eighth</type>
        </note>
      <note default-x="69.00" default-y="-423.62">
        <unpitched>
          <display-step>E</display-step>
          <display-octave>4</display-octave>
          </unpitched>
        <duration>2</duration>
        <instrument id="P6-I81"/>
        <voice>1</voice>
        <type>32nd</type>
        <stem>down</stem>
        <notehead>x</notehead>
        <beam number="1">begin</beam>
        <beam number="2">begin</beam>
        <beam number="3">forward hook</beam>
        </note>
    </measure>
  </part>
</score-partwise>
"""


testContentV3 =
    """
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE score-partwise PUBLIC "-//Recordare//DTD MusicXML 3.1 Partwise//EN" "http://www.musicxml.org/dtds/partwise.dtd">
<score-partwise version="3.1">
  <identification>
    <encoding>
      <software>MuseScore 3.6.0</software>
      <encoding-date>2022-01-05</encoding-date>
      <supports element="accidental" type="yes"/>
      <supports element="beam" type="yes"/>
      <supports element="print" attribute="new-page" type="yes" value="yes"/>
      <supports element="print" attribute="new-system" type="yes" value="yes"/>
      <supports element="stem" type="yes"/>
      </encoding>
    </identification>
  <defaults>
    <scaling>
      <millimeters>6.99911</millimeters>
      <tenths>40</tenths>
      </scaling>
    <page-layout>
      <page-height>1596.77</page-height>
      <page-width>1233.87</page-width>
      <page-margins type="even">
        <left-margin>85.7252</left-margin>
        <right-margin>85.7252</right-margin>
        <top-margin>85.7252</top-margin>
        <bottom-margin>85.7252</bottom-margin>
        </page-margins>
      <page-margins type="odd">
        <left-margin>85.7252</left-margin>
        <right-margin>85.7252</right-margin>
        <top-margin>85.7252</top-margin>
        <bottom-margin>85.7252</bottom-margin>
        </page-margins>
      </page-layout>
    <word-font font-family="Edwin" font-size="10"/>
    <lyric-font font-family="Edwin" font-size="10"/>
    </defaults>
  <part-list>
    <part-group type="start" number="1">
      <group-symbol>bracket</group-symbol>
      </part-group>
    <part-group type="start" number="2">
      <group-symbol>square</group-symbol>
      </part-group>
    <score-part id="P1">
      <part-name>Abs1</part-name>
      <part-abbreviation>A1</part-abbreviation>
      <score-instrument id="P1-I81">
        <instrument-name>Mute Triangle</instrument-name>
        </score-instrument>
      <score-instrument id="P1-I82">
        <instrument-name>Open Triangle</instrument-name>
        </score-instrument>
      <midi-device port="1"></midi-device>
      <midi-instrument id="P1-I81">
        <midi-channel>10</midi-channel>
        <midi-program>49</midi-program>
        <midi-unpitched>81</midi-unpitched>
        <volume>78.7402</volume>
        <pan>0</pan>
        </midi-instrument>
      <midi-instrument id="P1-I82">
        <midi-channel>10</midi-channel>
        <midi-program>49</midi-program>
        <midi-unpitched>82</midi-unpitched>
        <volume>78.7402</volume>
        <pan>0</pan>
        </midi-instrument>
      </score-part>
    <score-part id="P2">
      <part-name>Abs2</part-name>
      <part-abbreviation>A2</part-abbreviation>
      <score-instrument id="P2-I81">
        <instrument-name>Mute Triangle</instrument-name>
        </score-instrument>
      <score-instrument id="P2-I82">
        <instrument-name>Open Triangle</instrument-name>
        </score-instrument>
      <midi-device port="2"></midi-device>
      <midi-instrument id="P2-I81">
        <midi-channel>10</midi-channel>
        <midi-program>49</midi-program>
        <midi-unpitched>81</midi-unpitched>
        <volume>78.7402</volume>
        <pan>0</pan>
        </midi-instrument>
      <midi-instrument id="P2-I82">
        <midi-channel>10</midi-channel>
        <midi-program>49</midi-program>
        <midi-unpitched>82</midi-unpitched>
        <volume>78.7402</volume>
        <pan>0</pan>
        </midi-instrument>
      </score-part>
    <score-part id="P3">
      <part-name>LeftBack</part-name>
      <part-abbreviation>LBack</part-abbreviation>
      <score-instrument id="P3-I81">
        <instrument-name>Mute Triangle</instrument-name>
        </score-instrument>
      <score-instrument id="P3-I82">
        <instrument-name>Open Triangle</instrument-name>
        </score-instrument>
      <midi-device port="3"></midi-device>
      <midi-instrument id="P3-I81">
        <midi-channel>10</midi-channel>
        <midi-program>49</midi-program>
        <midi-unpitched>81</midi-unpitched>
        <volume>78.7402</volume>
        <pan>0</pan>
        </midi-instrument>
      <midi-instrument id="P3-I82">
        <midi-channel>10</midi-channel>
        <midi-program>49</midi-program>
        <midi-unpitched>82</midi-unpitched>
        <volume>78.7402</volume>
        <pan>0</pan>
        </midi-instrument>
      </score-part>
    <score-part id="P4">
      <part-name>RightBack</part-name>
      <part-abbreviation>RBack</part-abbreviation>
      <score-instrument id="P4-I81">
        <instrument-name>Mute Triangle</instrument-name>
        </score-instrument>
      <score-instrument id="P4-I82">
        <instrument-name>Open Triangle</instrument-name>
        </score-instrument>
      <midi-device port="4"></midi-device>
      <midi-instrument id="P4-I81">
        <midi-channel>10</midi-channel>
        <midi-program>49</midi-program>
        <midi-unpitched>81</midi-unpitched>
        <volume>78.7402</volume>
        <pan>0</pan>
        </midi-instrument>
      <midi-instrument id="P4-I82">
        <midi-channel>10</midi-channel>
        <midi-program>49</midi-program>
        <midi-unpitched>82</midi-unpitched>
        <volume>78.7402</volume>
        <pan>0</pan>
        </midi-instrument>
      </score-part>
    <score-part id="P5">
      <part-name>Spine</part-name>
      <part-abbreviation>Spine</part-abbreviation>
      <score-instrument id="P5-I81">
        <instrument-name>Mute Triangle</instrument-name>
        </score-instrument>
      <score-instrument id="P5-I82">
        <instrument-name>Open Triangle</instrument-name>
        </score-instrument>
      <midi-device port="5"></midi-device>
      <midi-instrument id="P5-I81">
        <midi-channel>10</midi-channel>
        <midi-program>49</midi-program>
        <midi-unpitched>81</midi-unpitched>
        <volume>78.7402</volume>
        <pan>0</pan>
        </midi-instrument>
      <midi-instrument id="P5-I82">
        <midi-channel>10</midi-channel>
        <midi-program>49</midi-program>
        <midi-unpitched>82</midi-unpitched>
        <volume>78.7402</volume>
        <pan>0</pan>
        </midi-instrument>
      </score-part>
    <score-part id="P6">
      <part-name>LRib1</part-name>
      <part-abbreviation>LRib1</part-abbreviation>
      <score-instrument id="P6-I81">
        <instrument-name>Mute Triangle</instrument-name>
        </score-instrument>
      <score-instrument id="P6-I82">
        <instrument-name>Open Triangle</instrument-name>
        </score-instrument>
      <midi-device port="6"></midi-device>
      <midi-instrument id="P6-I81">
        <midi-channel>10</midi-channel>
        <midi-program>49</midi-program>
        <midi-unpitched>81</midi-unpitched>
        <volume>78.7402</volume>
        <pan>0</pan>
        </midi-instrument>
      <midi-instrument id="P6-I82">
        <midi-channel>10</midi-channel>
        <midi-program>49</midi-program>
        <midi-unpitched>82</midi-unpitched>
        <volume>78.7402</volume>
        <pan>0</pan>
        </midi-instrument>
      </score-part>
    <score-part id="P7">
      <part-name>LRib2</part-name>
      <part-abbreviation>LRib2</part-abbreviation>
      <score-instrument id="P7-I81">
        <instrument-name>Mute Triangle</instrument-name>
        </score-instrument>
      <score-instrument id="P7-I82">
        <instrument-name>Open Triangle</instrument-name>
        </score-instrument>
      <midi-device port="7"></midi-device>
      <midi-instrument id="P7-I81">
        <midi-channel>10</midi-channel>
        <midi-program>49</midi-program>
        <midi-unpitched>81</midi-unpitched>
        <volume>78.7402</volume>
        <pan>0</pan>
        </midi-instrument>
      <midi-instrument id="P7-I82">
        <midi-channel>10</midi-channel>
        <midi-program>49</midi-program>
        <midi-unpitched>82</midi-unpitched>
        <volume>78.7402</volume>
        <pan>0</pan>
        </midi-instrument>
      </score-part>
    <score-part id="P8">
      <part-name>LRib3</part-name>
      <part-abbreviation>LRib3</part-abbreviation>
      <score-instrument id="P8-I81">
        <instrument-name>Mute Triangle</instrument-name>
        </score-instrument>
      <score-instrument id="P8-I82">
        <instrument-name>Open Triangle</instrument-name>
        </score-instrument>
      <midi-device port="8"></midi-device>
      <midi-instrument id="P8-I81">
        <midi-channel>10</midi-channel>
        <midi-program>49</midi-program>
        <midi-unpitched>81</midi-unpitched>
        <volume>78.7402</volume>
        <pan>0</pan>
        </midi-instrument>
      <midi-instrument id="P8-I82">
        <midi-channel>10</midi-channel>
        <midi-program>49</midi-program>
        <midi-unpitched>82</midi-unpitched>
        <volume>78.7402</volume>
        <pan>0</pan>
        </midi-instrument>
      </score-part>
    <score-part id="P9">
      <part-name>Palm</part-name>
      <part-abbreviation>Palm</part-abbreviation>
      <score-instrument id="P9-I81">
        <instrument-name>Mute Triangle</instrument-name>
        </score-instrument>
      <score-instrument id="P9-I82">
        <instrument-name>Open Triangle</instrument-name>
        </score-instrument>
      <midi-device port="9"></midi-device>
      <midi-instrument id="P9-I81">
        <midi-channel>10</midi-channel>
        <midi-program>49</midi-program>
        <midi-unpitched>81</midi-unpitched>
        <volume>78.7402</volume>
        <pan>0</pan>
        </midi-instrument>
      <midi-instrument id="P9-I82">
        <midi-channel>10</midi-channel>
        <midi-program>49</midi-program>
        <midi-unpitched>82</midi-unpitched>
        <volume>78.7402</volume>
        <pan>0</pan>
        </midi-instrument>
      </score-part>
    <score-part id="P10">
      <part-name>SPARE</part-name>
      <part-abbreviation>SPARE</part-abbreviation>
      <score-instrument id="P10-I81">
        <instrument-name>Mute Triangle</instrument-name>
        </score-instrument>
      <score-instrument id="P10-I82">
        <instrument-name>Open Triangle</instrument-name>
        </score-instrument>
      <midi-device port="10"></midi-device>
      <midi-instrument id="P10-I81">
        <midi-channel>10</midi-channel>
        <midi-program>49</midi-program>
        <midi-unpitched>81</midi-unpitched>
        <volume>78.7402</volume>
        <pan>0</pan>
        </midi-instrument>
      <midi-instrument id="P10-I82">
        <midi-channel>10</midi-channel>
        <midi-program>49</midi-program>
        <midi-unpitched>82</midi-unpitched>
        <volume>78.7402</volume>
        <pan>0</pan>
        </midi-instrument>
      </score-part>
    <score-part id="P11">
      <part-name>RRib1</part-name>
      <part-abbreviation>RRib1</part-abbreviation>
      <score-instrument id="P11-I81">
        <instrument-name>Mute Triangle</instrument-name>
        </score-instrument>
      <score-instrument id="P11-I82">
        <instrument-name>Open Triangle</instrument-name>
        </score-instrument>
      <midi-device port="11"></midi-device>
      <midi-instrument id="P11-I81">
        <midi-channel>10</midi-channel>
        <midi-program>49</midi-program>
        <midi-unpitched>81</midi-unpitched>
        <volume>78.7402</volume>
        <pan>0</pan>
        </midi-instrument>
      <midi-instrument id="P11-I82">
        <midi-channel>10</midi-channel>
        <midi-program>49</midi-program>
        <midi-unpitched>82</midi-unpitched>
        <volume>78.7402</volume>
        <pan>0</pan>
        </midi-instrument>
      </score-part>
    <score-part id="P12">
      <part-name>RRib2</part-name>
      <part-abbreviation>RRib2
</part-abbreviation>
      <score-instrument id="P12-I81">
        <instrument-name>Mute Triangle</instrument-name>
        </score-instrument>
      <score-instrument id="P12-I82">
        <instrument-name>Open Triangle</instrument-name>
        </score-instrument>
      <midi-device port="12"></midi-device>
      <midi-instrument id="P12-I81">
        <midi-channel>10</midi-channel>
        <midi-program>49</midi-program>
        <midi-unpitched>81</midi-unpitched>
        <volume>78.7402</volume>
        <pan>0</pan>
        </midi-instrument>
      <midi-instrument id="P12-I82">
        <midi-channel>10</midi-channel>
        <midi-program>49</midi-program>
        <midi-unpitched>82</midi-unpitched>
        <volume>78.7402</volume>
        <pan>0</pan>
        </midi-instrument>
      </score-part>
    <score-part id="P13">
      <part-name>RRib3</part-name>
      <part-abbreviation>RRib3</part-abbreviation>
      <score-instrument id="P13-I81">
        <instrument-name>Mute Triangle</instrument-name>
        </score-instrument>
      <score-instrument id="P13-I82">
        <instrument-name>Open Triangle</instrument-name>
        </score-instrument>
      <midi-device port="13"></midi-device>
      <midi-instrument id="P13-I81">
        <midi-channel>10</midi-channel>
        <midi-program>49</midi-program>
        <midi-unpitched>81</midi-unpitched>
        <volume>78.7402</volume>
        <pan>0</pan>
        </midi-instrument>
      <midi-instrument id="P13-I82">
        <midi-channel>10</midi-channel>
        <midi-program>49</midi-program>
        <midi-unpitched>82</midi-unpitched>
        <volume>78.7402</volume>
        <pan>0</pan>
        </midi-instrument>
      </score-part>
    <score-part id="P14">
      <part-name>Exhaust1</part-name>
      <part-abbreviation>Exhaust1</part-abbreviation>
      <score-instrument id="P14-I81">
        <instrument-name>Mute Triangle</instrument-name>
        </score-instrument>
      <score-instrument id="P14-I82">
        <instrument-name>Open Triangle</instrument-name>
        </score-instrument>
      <midi-device port="14"></midi-device>
      <midi-instrument id="P14-I81">
        <midi-channel>10</midi-channel>
        <midi-program>49</midi-program>
        <midi-unpitched>81</midi-unpitched>
        <volume>78.7402</volume>
        <pan>0</pan>
        </midi-instrument>
      <midi-instrument id="P14-I82">
        <midi-channel>10</midi-channel>
        <midi-program>49</midi-program>
        <midi-unpitched>82</midi-unpitched>
        <volume>78.7402</volume>
        <pan>0</pan>
        </midi-instrument>
      </score-part>
    <score-part id="P15">
      <part-name>Exhaust2</part-name>
      <part-abbreviation>Exhaust2</part-abbreviation>
      <score-instrument id="P15-I81">
        <instrument-name>Mute Triangle</instrument-name>
        </score-instrument>
      <score-instrument id="P15-I82">
        <instrument-name>Open Triangle</instrument-name>
        </score-instrument>
      <midi-device port="15"></midi-device>
      <midi-instrument id="P15-I81">
        <midi-channel>10</midi-channel>
        <midi-program>49</midi-program>
        <midi-unpitched>81</midi-unpitched>
        <volume>78.7402</volume>
        <pan>0</pan>
        </midi-instrument>
      <midi-instrument id="P15-I82">
        <midi-channel>10</midi-channel>
        <midi-program>49</midi-program>
        <midi-unpitched>82</midi-unpitched>
        <volume>78.7402</volume>
        <pan>0</pan>
        </midi-instrument>
      </score-part>
    <part-group type="stop" number="2"/>
    <part-group type="stop" number="1"/>
    </part-list>
  <part id="P1">
    <measure number="1" width="266.54">
      <print>
        <system-layout>
          <system-margins>
            <left-margin>121.68</left-margin>
            <right-margin>0.00</right-margin>
            </system-margins>
          <top-system-distance>93.98</top-system-distance>
          </system-layout>
        </print>
      <attributes>
        <divisions>16</divisions>
        <key>
          <fifths>0</fifths>
          </key>
        <time>
          <beats>3</beats>
          <beat-type>8</beat-type>
          </time>
        <clef>
          <sign>percussion</sign>
          <line>2</line>
          </clef>
        <staff-details>
          <staff-lines>1</staff-lines>
          </staff-details>
        </attributes>
      <direction placement="above">
        <direction-type>
          <metronome parentheses="no" default-x="-35.72" relative-x="37.04" relative-y="69.39">
            <beat-unit>eighth</beat-unit>
            <per-minute>80</per-minute>
            </metronome>
          </direction-type>
        <sound tempo="40"/>
        </direction>
      <direction placement="below">
        <direction-type>
          <dynamics default-x="3.25" default-y="-26.84" relative-y="-25.00">
            <p/>
            </dynamics>
          </direction-type>
        <sound dynamics="54.44"/>
        </direction>
      <note default-x="69.00" default-y="0.00">
        <unpitched>
          <display-step>E</display-step>
          <display-octave>4</display-octave>
          </unpitched>
        <duration>14</duration>
        <instrument id="P1-I82"/>
        <voice>1</voice>
        <type>eighth</type>
        <dot/>
        <dot/>
        <stem>down</stem>
        </note>
      <note>
        <rest/>
        <duration>8</duration>
        <voice>1</voice>
        <type>eighth</type>
        </note>
      <note>
        <rest/>
        <duration>2</duration>
        <voice>1</voice>
        <type>32nd</type>
        </note>
      </measure>
    <measure number="2" width="210.54">
      <direction placement="below">
        <direction-type>
          <dynamics default-x="3.25" default-y="-26.84" relative-y="-25.00">
            <p/>
            </dynamics>
          </direction-type>
        <sound dynamics="54.44"/>
        </direction>
      <note default-x="13.00" default-y="0.00">
        <unpitched>
          <display-step>E</display-step>
          <display-octave>4</display-octave>
          </unpitched>
        <duration>14</duration>
        <instrument id="P1-I82"/>
        <voice>1</voice>
        <type>eighth</type>
        <dot/>
        <dot/>
        <stem>down</stem>
        </note>
      <note>
        <rest/>
        <duration>8</duration>
        <voice>1</voice>
        <type>eighth</type>
        </note>
      <note>
        <rest/>
        <duration>2</duration>
        <voice>1</voice>
        <type>32nd</type>
        </note>
      </measure>
    <measure number="3" width="210.54">
      <direction placement="below">
        <direction-type>
          <dynamics default-x="3.25" default-y="-26.84" relative-y="-25.00">
            <p/>
            </dynamics>
          </direction-type>
        <sound dynamics="54.44"/>
        </direction>
      <note default-x="13.00" default-y="0.00">
        <unpitched>
          <display-step>E</display-step>
          <display-octave>4</display-octave>
          </unpitched>
        <duration>14</duration>
        <instrument id="P1-I82"/>
        <voice>1</voice>
        <type>eighth</type>
        <dot/>
        <dot/>
        <stem>down</stem>
        </note>
      <note>
        <rest/>
        <duration>8</duration>
        <voice>1</voice>
        <type>eighth</type>
        </note>
      <note>
        <rest/>
        <duration>2</duration>
        <voice>1</voice>
        <type>32nd</type>
        </note>
      </measure>
    <measure number="4" width="219.39">
      <direction placement="below">
        <direction-type>
          <dynamics default-x="3.25" default-y="-26.84" relative-y="-25.00">
            <p/>
            </dynamics>
          </direction-type>
        <sound dynamics="54.44"/>
        </direction>
      <note default-x="13.00" default-y="0.00">
        <unpitched>
          <display-step>E</display-step>
          <display-octave>4</display-octave>
          </unpitched>
        <duration>14</duration>
        <instrument id="P1-I82"/>
        <voice>1</voice>
        <type>eighth</type>
        <dot/>
        <dot/>
        <stem>down</stem>
        </note>
      <note>
        <rest/>
        <duration>8</duration>
        <voice>1</voice>
        <type>eighth</type>
        </note>
      <note>
        <rest/>
        <duration>2</duration>
        <voice>1</voice>
        <type>32nd</type>
        </note>
      <barline location="right">
        <bar-style>light-heavy</bar-style>
        </barline>
      </measure>
    </part>
  <part id="P2">
    <measure number="1" width="266.54">
      <print>
        <staff-layout number="1">
          <staff-distance>44.72</staff-distance>
          </staff-layout>
        </print>
      <attributes>
        <divisions>16</divisions>
        <key>
          <fifths>0</fifths>
          </key>
        <time>
          <beats>3</beats>
          <beat-type>8</beat-type>
          </time>
        <clef>
          <sign>percussion</sign>
          <line>2</line>
          </clef>
        <staff-details>
          <staff-lines>1</staff-lines>
          </staff-details>
        </attributes>
      <direction placement="below">
        <direction-type>
          <dynamics default-x="3.25" default-y="-26.84" relative-y="-25.00">
            <p/>
            </dynamics>
          </direction-type>
        <sound dynamics="54.44"/>
        </direction>
      <note default-x="69.00" default-y="-84.72">
        <unpitched>
          <display-step>E</display-step>
          <display-octave>4</display-octave>
          </unpitched>
        <duration>14</duration>
        <instrument id="P2-I82"/>
        <voice>1</voice>
        <type>eighth</type>
        <dot/>
        <dot/>
        <stem>down</stem>
        </note>
      <note>
        <rest/>
        <duration>8</duration>
        <voice>1</voice>
        <type>eighth</type>
        </note>
      <note>
        <rest/>
        <duration>2</duration>
        <voice>1</voice>
        <type>32nd</type>
        </note>
      </measure>
    <measure number="2" width="210.54">
      <direction placement="below">
        <direction-type>
          <dynamics default-x="3.25" default-y="-26.84" relative-y="-25.00">
            <p/>
            </dynamics>
          </direction-type>
        <sound dynamics="54.44"/>
        </direction>
      <note default-x="13.00" default-y="-84.72">
        <unpitched>
          <display-step>E</display-step>
          <display-octave>4</display-octave>
          </unpitched>
        <duration>14</duration>
        <instrument id="P2-I82"/>
        <voice>1</voice>
        <type>eighth</type>
        <dot/>
        <dot/>
        <stem>down</stem>
        </note>
      <note>
        <rest/>
        <duration>8</duration>
        <voice>1</voice>
        <type>eighth</type>
        </note>
      <note>
        <rest/>
        <duration>2</duration>
        <voice>1</voice>
        <type>32nd</type>
        </note>
      </measure>
    <measure number="3" width="210.54">
      <direction placement="below">
        <direction-type>
          <dynamics default-x="3.25" default-y="-26.84" relative-y="-25.00">
            <p/>
            </dynamics>
          </direction-type>
        <sound dynamics="54.44"/>
        </direction>
      <note default-x="13.00" default-y="-84.72">
        <unpitched>
          <display-step>E</display-step>
          <display-octave>4</display-octave>
          </unpitched>
        <duration>14</duration>
        <instrument id="P2-I82"/>
        <voice>1</voice>
        <type>eighth</type>
        <dot/>
        <dot/>
        <stem>down</stem>
        </note>
      <note>
        <rest/>
        <duration>8</duration>
        <voice>1</voice>
        <type>eighth</type>
        </note>
      <note>
        <rest/>
        <duration>2</duration>
        <voice>1</voice>
        <type>32nd</type>
        </note>
      </measure>
    <measure number="4" width="219.39">
      <direction placement="below">
        <direction-type>
          <dynamics default-x="3.25" default-y="-26.84" relative-y="-25.00">
            <p/>
            </dynamics>
          </direction-type>
        <sound dynamics="54.44"/>
        </direction>
      <note default-x="13.00" default-y="-84.72">
        <unpitched>
          <display-step>E</display-step>
          <display-octave>4</display-octave>
          </unpitched>
        <duration>14</duration>
        <instrument id="P2-I82"/>
        <voice>1</voice>
        <type>eighth</type>
        <dot/>
        <dot/>
        <stem>down</stem>
        </note>
      <note>
        <rest/>
        <duration>8</duration>
        <voice>1</voice>
        <type>eighth</type>
        </note>
      <note>
        <rest/>
        <duration>2</duration>
        <voice>1</voice>
        <type>32nd</type>
        </note>
      <barline location="right">
        <bar-style>light-heavy</bar-style>
        </barline>
      </measure>
    </part>
  <part id="P3">
    <measure number="1" width="266.54">
      <print>
        <staff-layout number="1">
          <staff-distance>44.72</staff-distance>
          </staff-layout>
        </print>
      <attributes>
        <divisions>16</divisions>
        <key>
          <fifths>0</fifths>
          </key>
        <time>
          <beats>3</beats>
          <beat-type>8</beat-type>
          </time>
        <clef>
          <sign>percussion</sign>
          <line>2</line>
          </clef>
        <staff-details>
          <staff-lines>1</staff-lines>
          </staff-details>
        </attributes>
      <direction placement="below">
        <direction-type>
          <dynamics default-x="3.25" default-y="-26.84" relative-y="-25.00">
            <p/>
            </dynamics>
          </direction-type>
        <sound dynamics="54.44"/>
        </direction>
      <note default-x="69.00" default-y="-169.45">
        <unpitched>
          <display-step>E</display-step>
          <display-octave>4</display-octave>
          </unpitched>
        <duration>14</duration>
        <instrument id="P3-I82"/>
        <voice>1</voice>
        <type>eighth</type>
        <dot/>
        <dot/>
        <stem>down</stem>
        </note>
      <note>
        <rest/>
        <duration>8</duration>
        <voice>1</voice>
        <type>eighth</type>
        </note>
      <note>
        <rest/>
        <duration>2</duration>
        <voice>1</voice>
        <type>32nd</type>
        </note>
      </measure>
    <measure number="2" width="210.54">
      <direction placement="below">
        <direction-type>
          <dynamics default-x="3.25" default-y="-26.84" relative-y="-25.00">
            <p/>
            </dynamics>
          </direction-type>
        <sound dynamics="54.44"/>
        </direction>
      <note default-x="13.00" default-y="-169.45">
        <unpitched>
          <display-step>E</display-step>
          <display-octave>4</display-octave>
          </unpitched>
        <duration>14</duration>
        <instrument id="P3-I82"/>
        <voice>1</voice>
        <type>eighth</type>
        <dot/>
        <dot/>
        <stem>down</stem>
        </note>
      <note>
        <rest/>
        <duration>8</duration>
        <voice>1</voice>
        <type>eighth</type>
        </note>
      <note>
        <rest/>
        <duration>2</duration>
        <voice>1</voice>
        <type>32nd</type>
        </note>
      </measure>
    <measure number="3" width="210.54">
      <direction placement="below">
        <direction-type>
          <dynamics default-x="3.25" default-y="-26.84" relative-y="-25.00">
            <p/>
            </dynamics>
          </direction-type>
        <sound dynamics="54.44"/>
        </direction>
      <note default-x="13.00" default-y="-169.45">
        <unpitched>
          <display-step>E</display-step>
          <display-octave>4</display-octave>
          </unpitched>
        <duration>14</duration>
        <instrument id="P3-I82"/>
        <voice>1</voice>
        <type>eighth</type>
        <dot/>
        <dot/>
        <stem>down</stem>
        </note>
      <note>
        <rest/>
        <duration>8</duration>
        <voice>1</voice>
        <type>eighth</type>
        </note>
      <note>
        <rest/>
        <duration>2</duration>
        <voice>1</voice>
        <type>32nd</type>
        </note>
      </measure>
    <measure number="4" width="219.39">
      <direction placement="below">
        <direction-type>
          <dynamics default-x="3.25" default-y="-26.84" relative-y="-25.00">
            <p/>
            </dynamics>
          </direction-type>
        <sound dynamics="54.44"/>
        </direction>
      <note default-x="13.00" default-y="-169.45">
        <unpitched>
          <display-step>E</display-step>
          <display-octave>4</display-octave>
          </unpitched>
        <duration>14</duration>
        <instrument id="P3-I82"/>
        <voice>1</voice>
        <type>eighth</type>
        <dot/>
        <dot/>
        <stem>down</stem>
        </note>
      <note>
        <rest/>
        <duration>8</duration>
        <voice>1</voice>
        <type>eighth</type>
        </note>
      <note>
        <rest/>
        <duration>2</duration>
        <voice>1</voice>
        <type>32nd</type>
        </note>
      <barline location="right">
        <bar-style>light-heavy</bar-style>
        </barline>
      </measure>
    </part>
  <part id="P4">
    <measure number="1" width="266.54">
      <print>
        <staff-layout number="1">
          <staff-distance>44.72</staff-distance>
          </staff-layout>
        </print>
      <attributes>
        <divisions>16</divisions>
        <key>
          <fifths>0</fifths>
          </key>
        <time>
          <beats>3</beats>
          <beat-type>8</beat-type>
          </time>
        <clef>
          <sign>percussion</sign>
          <line>2</line>
          </clef>
        <staff-details>
          <staff-lines>1</staff-lines>
          </staff-details>
        </attributes>
      <direction placement="below">
        <direction-type>
          <dynamics default-x="3.25" default-y="-26.84" relative-y="-25.00">
            <p/>
            </dynamics>
          </direction-type>
        <sound dynamics="54.44"/>
        </direction>
      <note default-x="69.00" default-y="-254.17">
        <unpitched>
          <display-step>E</display-step>
          <display-octave>4</display-octave>
          </unpitched>
        <duration>14</duration>
        <instrument id="P4-I82"/>
        <voice>1</voice>
        <type>eighth</type>
        <dot/>
        <dot/>
        <stem>down</stem>
        </note>
      <note>
        <rest/>
        <duration>8</duration>
        <voice>1</voice>
        <type>eighth</type>
        </note>
      <note>
        <rest/>
        <duration>2</duration>
        <voice>1</voice>
        <type>32nd</type>
        </note>
      </measure>
    <measure number="2" width="210.54">
      <direction placement="below">
        <direction-type>
          <dynamics default-x="3.25" default-y="-26.84" relative-y="-25.00">
            <p/>
            </dynamics>
          </direction-type>
        <sound dynamics="54.44"/>
        </direction>
      <note default-x="13.00" default-y="-254.17">
        <unpitched>
          <display-step>E</display-step>
          <display-octave>4</display-octave>
          </unpitched>
        <duration>14</duration>
        <instrument id="P4-I82"/>
        <voice>1</voice>
        <type>eighth</type>
        <dot/>
        <dot/>
        <stem>down</stem>
        </note>
      <note>
        <rest/>
        <duration>8</duration>
        <voice>1</voice>
        <type>eighth</type>
        </note>
      <note>
        <rest/>
        <duration>2</duration>
        <voice>1</voice>
        <type>32nd</type>
        </note>
      </measure>
    <measure number="3" width="210.54">
      <direction placement="below">
        <direction-type>
          <dynamics default-x="3.25" default-y="-26.84" relative-y="-25.00">
            <p/>
            </dynamics>
          </direction-type>
        <sound dynamics="54.44"/>
        </direction>
      <note default-x="13.00" default-y="-254.17">
        <unpitched>
          <display-step>E</display-step>
          <display-octave>4</display-octave>
          </unpitched>
        <duration>14</duration>
        <instrument id="P4-I82"/>
        <voice>1</voice>
        <type>eighth</type>
        <dot/>
        <dot/>
        <stem>down</stem>
        </note>
      <note>
        <rest/>
        <duration>8</duration>
        <voice>1</voice>
        <type>eighth</type>
        </note>
      <note>
        <rest/>
        <duration>2</duration>
        <voice>1</voice>
        <type>32nd</type>
        </note>
      </measure>
    <measure number="4" width="219.39">
      <direction placement="below">
        <direction-type>
          <dynamics default-x="3.25" default-y="-26.84" relative-y="-25.00">
            <p/>
            </dynamics>
          </direction-type>
        <sound dynamics="54.44"/>
        </direction>
      <note default-x="13.00" default-y="-254.17">
        <unpitched>
          <display-step>E</display-step>
          <display-octave>4</display-octave>
          </unpitched>
        <duration>14</duration>
        <instrument id="P4-I82"/>
        <voice>1</voice>
        <type>eighth</type>
        <dot/>
        <dot/>
        <stem>down</stem>
        </note>
      <note>
        <rest/>
        <duration>8</duration>
        <voice>1</voice>
        <type>eighth</type>
        </note>
      <note>
        <rest/>
        <duration>2</duration>
        <voice>1</voice>
        <type>32nd</type>
        </note>
      <barline location="right">
        <bar-style>light-heavy</bar-style>
        </barline>
      </measure>
    </part>
  <part id="P5">
    <measure number="1" width="266.54">
      <print>
        <staff-layout number="1">
          <staff-distance>44.72</staff-distance>
          </staff-layout>
        </print>
      <attributes>
        <divisions>16</divisions>
        <key>
          <fifths>0</fifths>
          </key>
        <time>
          <beats>3</beats>
          <beat-type>8</beat-type>
          </time>
        <clef>
          <sign>percussion</sign>
          <line>2</line>
          </clef>
        <staff-details>
          <staff-lines>1</staff-lines>
          </staff-details>
        </attributes>
      <direction placement="below">
        <direction-type>
          <dynamics default-x="3.25" default-y="-26.84" relative-y="-25.00">
            <p/>
            </dynamics>
          </direction-type>
        <sound dynamics="54.44"/>
        </direction>
      <note default-x="69.00" default-y="-338.89">
        <unpitched>
          <display-step>E</display-step>
          <display-octave>4</display-octave>
          </unpitched>
        <duration>12</duration>
        <instrument id="P5-I82"/>
        <voice>1</voice>
        <type>eighth</type>
        <dot/>
        <stem>down</stem>
        </note>
      <note>
        <rest/>
        <duration>12</duration>
        <voice>1</voice>
        <type>eighth</type>
        <dot/>
        </note>
      </measure>
    <measure number="2" width="210.54">
      <direction placement="below">
        <direction-type>
          <dynamics default-x="3.25" default-y="-26.84" relative-y="-25.00">
            <p/>
            </dynamics>
          </direction-type>
        <sound dynamics="54.44"/>
        </direction>
      <note default-x="13.00" default-y="-338.89">
        <unpitched>
          <display-step>E</display-step>
          <display-octave>4</display-octave>
          </unpitched>
        <duration>12</duration>
        <instrument id="P5-I82"/>
        <voice>1</voice>
        <type>eighth</type>
        <dot/>
        <stem>down</stem>
        </note>
      <note>
        <rest/>
        <duration>12</duration>
        <voice>1</voice>
        <type>eighth</type>
        <dot/>
        </note>
      </measure>
    <measure number="3" width="210.54">
      <direction placement="below">
        <direction-type>
          <dynamics default-x="3.25" default-y="-26.84" relative-y="-25.00">
            <p/>
            </dynamics>
          </direction-type>
        <sound dynamics="54.44"/>
        </direction>
      <note default-x="13.00" default-y="-338.89">
        <unpitched>
          <display-step>E</display-step>
          <display-octave>4</display-octave>
          </unpitched>
        <duration>12</duration>
        <instrument id="P5-I82"/>
        <voice>1</voice>
        <type>eighth</type>
        <dot/>
        <stem>down</stem>
        </note>
      <note>
        <rest/>
        <duration>12</duration>
        <voice>1</voice>
        <type>eighth</type>
        <dot/>
        </note>
      </measure>
    <measure number="4" width="219.39">
      <direction placement="below">
        <direction-type>
          <dynamics default-x="3.25" default-y="-26.84" relative-y="-25.00">
            <p/>
            </dynamics>
          </direction-type>
        <sound dynamics="54.44"/>
        </direction>
      <note default-x="13.00" default-y="-338.89">
        <unpitched>
          <display-step>E</display-step>
          <display-octave>4</display-octave>
          </unpitched>
        <duration>12</duration>
        <instrument id="P5-I82"/>
        <voice>1</voice>
        <type>eighth</type>
        <dot/>
        <stem>down</stem>
        </note>
      <note>
        <rest/>
        <duration>12</duration>
        <voice>1</voice>
        <type>eighth</type>
        <dot/>
        </note>
      <barline location="right">
        <bar-style>light-heavy</bar-style>
        </barline>
      </measure>
    </part>
  <part id="P6">
    <measure number="1" width="266.54">
      <print>
        <staff-layout number="1">
          <staff-distance>44.72</staff-distance>
          </staff-layout>
        </print>
      <attributes>
        <divisions>16</divisions>
        <key>
          <fifths>0</fifths>
          </key>
        <time>
          <beats>3</beats>
          <beat-type>8</beat-type>
          </time>
        <clef>
          <sign>percussion</sign>
          <line>2</line>
          </clef>
        <staff-details>
          <staff-lines>1</staff-lines>
          </staff-details>
        </attributes>
      <note default-x="69.00" default-y="-423.62">
        <unpitched>
          <display-step>E</display-step>
          <display-octave>4</display-octave>
          </unpitched>
        <duration>2</duration>
        <instrument id="P6-I81"/>
        <voice>1</voice>
        <type>32nd</type>
        <stem>down</stem>
        <notehead>x</notehead>
        <beam number="1">begin</beam>
        <beam number="2">begin</beam>
        <beam number="3">forward hook</beam>
        </note>
      <direction placement="below">
        <direction-type>
          <dynamics default-x="3.25" default-y="-31.29" relative-y="-25.00">
            <p/>
            </dynamics>
          </direction-type>
        <sound dynamics="54.44"/>
        </direction>
      <note default-x="98.99" default-y="-423.62">
        <unpitched>
          <display-step>E</display-step>
          <display-octave>4</display-octave>
          </unpitched>
        <duration>6</duration>
        <tie type="start"/>
        <instrument id="P6-I82"/>
        <voice>1</voice>
        <type>16th</type>
        <dot/>
        <stem>down</stem>
        <beam number="1">continue</beam>
        <beam number="2">continue</beam>
        <notations>
          <tied type="start"/>
          </notations>
        </note>
      <note default-x="131.82" default-y="-423.62">
        <unpitched>
          <display-step>E</display-step>
          <display-octave>4</display-octave>
          </unpitched>
        <duration>4</duration>
        <tie type="stop"/>
        <instrument id="P6-I82"/>
        <voice>1</voice>
        <type>16th</type>
        <stem>down</stem>
        <beam number="1">end</beam>
        <beam number="2">end</beam>
        <notations>
          <tied type="stop"/>
          </notations>
        </note>
      <note>
        <rest/>
        <duration>12</duration>
        <voice>1</voice>
        <type>eighth</type>
        <dot/>
        </note>
      </measure>
    <measure number="2" width="210.54">
      <note default-x="13.00" default-y="-423.62">
        <unpitched>
          <display-step>E</display-step>
          <display-octave>4</display-octave>
          </unpitched>
        <duration>2</duration>
        <instrument id="P6-I81"/>
        <voice>1</voice>
        <type>32nd</type>
        <stem>down</stem>
        <notehead>x</notehead>
        <beam number="1">begin</beam>
        <beam number="2">begin</beam>
        <beam number="3">forward hook</beam>
        </note>
      <direction placement="below">
        <direction-type>
          <dynamics default-x="3.25" default-y="-31.29" relative-y="-25.00">
            <p/>
            </dynamics>
          </direction-type>
        <sound dynamics="54.44"/>
        </direction>
      <note default-x="42.99" default-y="-423.62">
        <unpitched>
          <display-step>E</display-step>
          <display-octave>4</display-octave>
          </unpitched>
        <duration>6</duration>
        <tie type="start"/>
        <instrument id="P6-I82"/>
        <voice>1</voice>
        <type>16th</type>
        <dot/>
        <stem>down</stem>
        <beam number="1">continue</beam>
        <beam number="2">continue</beam>
        <notations>
          <tied type="start"/>
          </notations>
        </note>
      <note default-x="75.82" default-y="-423.62">
        <unpitched>
          <display-step>E</display-step>
          <display-octave>4</display-octave>
          </unpitched>
        <duration>4</duration>
        <tie type="stop"/>
        <instrument id="P6-I82"/>
        <voice>1</voice>
        <type>16th</type>
        <stem>down</stem>
        <beam number="1">end</beam>
        <beam number="2">end</beam>
        <notations>
          <tied type="stop"/>
          </notations>
        </note>
      <note>
        <rest/>
        <duration>12</duration>
        <voice>1</voice>
        <type>eighth</type>
        <dot/>
        </note>
      </measure>
    <measure number="3" width="210.54">
      <note default-x="13.00" default-y="-423.62">
        <unpitched>
          <display-step>E</display-step>
          <display-octave>4</display-octave>
          </unpitched>
        <duration>2</duration>
        <instrument id="P6-I81"/>
        <voice>1</voice>
        <type>32nd</type>
        <stem>down</stem>
        <notehead>x</notehead>
        <beam number="1">begin</beam>
        <beam number="2">begin</beam>
        <beam number="3">forward hook</beam>
        </note>
      <direction placement="below">
        <direction-type>
          <dynamics default-x="3.25" default-y="-31.29" relative-y="-25.00">
            <p/>
            </dynamics>
          </direction-type>
        <sound dynamics="54.44"/>
        </direction>
      <note default-x="42.99" default-y="-423.62">
        <unpitched>
          <display-step>E</display-step>
          <display-octave>4</display-octave>
          </unpitched>
        <duration>6</duration>
        <tie type="start"/>
        <instrument id="P6-I82"/>
        <voice>1</voice>
        <type>16th</type>
        <dot/>
        <stem>down</stem>
        <beam number="1">continue</beam>
        <beam number="2">continue</beam>
        <notations>
          <tied type="start"/>
          </notations>
        </note>
      <note default-x="75.82" default-y="-423.62">
        <unpitched>
          <display-step>E</display-step>
          <display-octave>4</display-octave>
          </unpitched>
        <duration>4</duration>
        <tie type="stop"/>
        <instrument id="P6-I82"/>
        <voice>1</voice>
        <type>16th</type>
        <stem>down</stem>
        <beam number="1">end</beam>
        <beam number="2">end</beam>
        <notations>
          <tied type="stop"/>
          </notations>
        </note>
      <note>
        <rest/>
        <duration>12</duration>
        <voice>1</voice>
        <type>eighth</type>
        <dot/>
        </note>
      </measure>
    <measure number="4" width="219.39">
      <note default-x="13.00" default-y="-423.62">
        <unpitched>
          <display-step>E</display-step>
          <display-octave>4</display-octave>
          </unpitched>
        <duration>2</duration>
        <instrument id="P6-I81"/>
        <voice>1</voice>
        <type>32nd</type>
        <stem>down</stem>
        <notehead>x</notehead>
        <beam number="1">begin</beam>
        <beam number="2">begin</beam>
        <beam number="3">forward hook</beam>
        </note>
      <direction placement="below">
        <direction-type>
          <dynamics default-x="3.25" default-y="-31.29" relative-y="-25.00">
            <p/>
            </dynamics>
          </direction-type>
        <sound dynamics="54.44"/>
        </direction>
      <note default-x="42.99" default-y="-423.62">
        <unpitched>
          <display-step>E</display-step>
          <display-octave>4</display-octave>
          </unpitched>
        <duration>6</duration>
        <tie type="start"/>
        <instrument id="P6-I82"/>
        <voice>1</voice>
        <type>16th</type>
        <dot/>
        <stem>down</stem>
        <beam number="1">continue</beam>
        <beam number="2">continue</beam>
        <notations>
          <tied type="start"/>
          </notations>
        </note>
      <note default-x="75.82" default-y="-423.62">
        <unpitched>
          <display-step>E</display-step>
          <display-octave>4</display-octave>
          </unpitched>
        <duration>4</duration>
        <tie type="stop"/>
        <instrument id="P6-I82"/>
        <voice>1</voice>
        <type>16th</type>
        <stem>down</stem>
        <beam number="1">end</beam>
        <beam number="2">end</beam>
        <notations>
          <tied type="stop"/>
          </notations>
        </note>
      <note>
        <rest/>
        <duration>12</duration>
        <voice>1</voice>
        <type>eighth</type>
        <dot/>
        </note>
      <barline location="right">
        <bar-style>light-heavy</bar-style>
        </barline>
      </measure>
    </part>
  <part id="P7">
    <measure number="1" width="266.54">
      <print>
        <staff-layout number="1">
          <staff-distance>44.72</staff-distance>
          </staff-layout>
        </print>
      <attributes>
        <divisions>16</divisions>
        <key>
          <fifths>0</fifths>
          </key>
        <time>
          <beats>3</beats>
          <beat-type>8</beat-type>
          </time>
        <clef>
          <sign>percussion</sign>
          <line>2</line>
          </clef>
        <staff-details>
          <staff-lines>1</staff-lines>
          </staff-details>
        </attributes>
      <note default-x="69.00" default-y="-508.34">
        <unpitched>
          <display-step>E</display-step>
          <display-octave>4</display-octave>
          </unpitched>
        <duration>1</duration>
        <instrument id="P7-I81"/>
        <voice>1</voice>
        <type>64th</type>
        <stem>down</stem>
        <notehead>x</notehead>
        <beam number="1">begin</beam>
        <beam number="2">begin</beam>
        <beam number="3">forward hook</beam>
        <beam number="4">forward hook</beam>
        </note>
      <direction placement="below">
        <direction-type>
          <dynamics default-x="3.25" default-y="-41.29" relative-y="-25.00">
            <mp/>
            </dynamics>
          </direction-type>
        <sound dynamics="71.11"/>
        </direction>
      <note default-x="84.00" default-y="-508.34">
        <unpitched>
          <display-step>E</display-step>
          <display-octave>4</display-octave>
          </unpitched>
        <duration>7</duration>
        <instrument id="P7-I82"/>
        <voice>1</voice>
        <type>16th</type>
        <dot/>
        <dot/>
        <stem>down</stem>
        <beam number="1">end</beam>
        <beam number="2">end</beam>
        </note>
      <note>
        <rest/>
        <duration>8</duration>
        <voice>1</voice>
        <type>eighth</type>
        </note>
      <note>
        <rest/>
        <duration>8</duration>
        <voice>1</voice>
        <type>eighth</type>
        </note>
      </measure>
    <measure number="2" width="210.54">
      <note default-x="13.00" default-y="-508.34">
        <unpitched>
          <display-step>E</display-step>
          <display-octave>4</display-octave>
          </unpitched>
        <duration>1</duration>
        <instrument id="P7-I81"/>
        <voice>1</voice>
        <type>64th</type>
        <stem>down</stem>
        <notehead>x</notehead>
        <beam number="1">begin</beam>
        <beam number="2">begin</beam>
        <beam number="3">forward hook</beam>
        <beam number="4">forward hook</beam>
        </note>
      <direction placement="below">
        <direction-type>
          <dynamics default-x="3.25" default-y="-41.29" relative-y="-25.00">
            <mp/>
            </dynamics>
          </direction-type>
        <sound dynamics="71.11"/>
        </direction>
      <note default-x="28.00" default-y="-508.34">
        <unpitched>
          <display-step>E</display-step>
          <display-octave>4</display-octave>
          </unpitched>
        <duration>7</duration>
        <instrument id="P7-I82"/>
        <voice>1</voice>
        <type>16th</type>
        <dot/>
        <dot/>
        <stem>down</stem>
        <beam number="1">end</beam>
        <beam number="2">end</beam>
        </note>
      <note>
        <rest/>
        <duration>8</duration>
        <voice>1</voice>
        <type>eighth</type>
        </note>
      <note>
        <rest/>
        <duration>8</duration>
        <voice>1</voice>
        <type>eighth</type>
        </note>
      </measure>
    <measure number="3" width="210.54">
      <note default-x="13.00" default-y="-508.34">
        <unpitched>
          <display-step>E</display-step>
          <display-octave>4</display-octave>
          </unpitched>
        <duration>1</duration>
        <instrument id="P7-I81"/>
        <voice>1</voice>
        <type>64th</type>
        <stem>down</stem>
        <notehead>x</notehead>
        <beam number="1">begin</beam>
        <beam number="2">begin</beam>
        <beam number="3">forward hook</beam>
        <beam number="4">forward hook</beam>
        </note>
      <direction placement="below">
        <direction-type>
          <dynamics default-x="3.25" default-y="-41.29" relative-y="-25.00">
            <mp/>
            </dynamics>
          </direction-type>
        <sound dynamics="71.11"/>
        </direction>
      <note default-x="28.00" default-y="-508.34">
        <unpitched>
          <display-step>E</display-step>
          <display-octave>4</display-octave>
          </unpitched>
        <duration>7</duration>
        <instrument id="P7-I82"/>
        <voice>1</voice>
        <type>16th</type>
        <dot/>
        <dot/>
        <stem>down</stem>
        <beam number="1">end</beam>
        <beam number="2">end</beam>
        </note>
      <note>
        <rest/>
        <duration>8</duration>
        <voice>1</voice>
        <type>eighth</type>
        </note>
      <note>
        <rest/>
        <duration>8</duration>
        <voice>1</voice>
        <type>eighth</type>
        </note>
      </measure>
    <measure number="4" width="219.39">
      <note default-x="13.00" default-y="-508.34">
        <unpitched>
          <display-step>E</display-step>
          <display-octave>4</display-octave>
          </unpitched>
        <duration>1</duration>
        <instrument id="P7-I81"/>
        <voice>1</voice>
        <type>64th</type>
        <stem>down</stem>
        <notehead>x</notehead>
        <beam number="1">begin</beam>
        <beam number="2">begin</beam>
        <beam number="3">forward hook</beam>
        <beam number="4">forward hook</beam>
        </note>
      <direction placement="below">
        <direction-type>
          <dynamics default-x="3.25" default-y="-41.29" relative-y="-25.00">
            <mp/>
            </dynamics>
          </direction-type>
        <sound dynamics="71.11"/>
        </direction>
      <note default-x="28.00" default-y="-508.34">
        <unpitched>
          <display-step>E</display-step>
          <display-octave>4</display-octave>
          </unpitched>
        <duration>7</duration>
        <instrument id="P7-I82"/>
        <voice>1</voice>
        <type>16th</type>
        <dot/>
        <dot/>
        <stem>down</stem>
        <beam number="1">end</beam>
        <beam number="2">end</beam>
        </note>
      <note>
        <rest/>
        <duration>8</duration>
        <voice>1</voice>
        <type>eighth</type>
        </note>
      <note>
        <rest/>
        <duration>8</duration>
        <voice>1</voice>
        <type>eighth</type>
        </note>
      <barline location="right">
        <bar-style>light-heavy</bar-style>
        </barline>
      </measure>
    </part>
  <part id="P8">
    <measure number="1" width="266.54">
      <print>
        <staff-layout number="1">
          <staff-distance>48.70</staff-distance>
          </staff-layout>
        </print>
      <attributes>
        <divisions>16</divisions>
        <key>
          <fifths>0</fifths>
          </key>
        <time>
          <beats>3</beats>
          <beat-type>8</beat-type>
          </time>
        <clef>
          <sign>percussion</sign>
          <line>2</line>
          </clef>
        <staff-details>
          <staff-lines>1</staff-lines>
          </staff-details>
        </attributes>
      <direction placement="below">
        <direction-type>
          <dynamics default-x="3.25" default-y="-33.29" relative-y="-25.00">
            <mf/>
            </dynamics>
          </direction-type>
        <sound dynamics="88.89"/>
        </direction>
      <note default-x="69.00" default-y="-597.03">
        <unpitched>
          <display-step>E</display-step>
          <display-octave>4</display-octave>
          </unpitched>
        <duration>12</duration>
        <instrument id="P8-I82"/>
        <voice>1</voice>
        <type>eighth</type>
        <dot/>
        <stem>down</stem>
        </note>
      <note>
        <rest/>
        <duration>12</duration>
        <voice>1</voice>
        <type>eighth</type>
        <dot/>
        </note>
      </measure>
    <measure number="2" width="210.54">
      <direction placement="below">
        <direction-type>
          <dynamics default-x="3.25" default-y="-33.29" relative-y="-25.00">
            <mf/>
            </dynamics>
          </direction-type>
        <sound dynamics="88.89"/>
        </direction>
      <note default-x="13.00" default-y="-597.03">
        <unpitched>
          <display-step>E</display-step>
          <display-octave>4</display-octave>
          </unpitched>
        <duration>12</duration>
        <instrument id="P8-I82"/>
        <voice>1</voice>
        <type>eighth</type>
        <dot/>
        <stem>down</stem>
        </note>
      <note>
        <rest/>
        <duration>12</duration>
        <voice>1</voice>
        <type>eighth</type>
        <dot/>
        </note>
      </measure>
    <measure number="3" width="210.54">
      <direction placement="below">
        <direction-type>
          <dynamics default-x="3.25" default-y="-33.29" relative-y="-25.00">
            <mf/>
            </dynamics>
          </direction-type>
        <sound dynamics="88.89"/>
        </direction>
      <note default-x="13.00" default-y="-597.03">
        <unpitched>
          <display-step>E</display-step>
          <display-octave>4</display-octave>
          </unpitched>
        <duration>12</duration>
        <instrument id="P8-I82"/>
        <voice>1</voice>
        <type>eighth</type>
        <dot/>
        <stem>down</stem>
        </note>
      <note>
        <rest/>
        <duration>12</duration>
        <voice>1</voice>
        <type>eighth</type>
        <dot/>
        </note>
      </measure>
    <measure number="4" width="219.39">
      <direction placement="below">
        <direction-type>
          <dynamics default-x="3.25" default-y="-33.29" relative-y="-25.00">
            <mf/>
            </dynamics>
          </direction-type>
        <sound dynamics="88.89"/>
        </direction>
      <note default-x="13.00" default-y="-597.03">
        <unpitched>
          <display-step>E</display-step>
          <display-octave>4</display-octave>
          </unpitched>
        <duration>12</duration>
        <instrument id="P8-I82"/>
        <voice>1</voice>
        <type>eighth</type>
        <dot/>
        <stem>down</stem>
        </note>
      <note>
        <rest/>
        <duration>12</duration>
        <voice>1</voice>
        <type>eighth</type>
        <dot/>
        </note>
      <barline location="right">
        <bar-style>light-heavy</bar-style>
        </barline>
      </measure>
    </part>
  <part id="P9">
    <measure number="1" width="266.54">
      <print>
        <staff-layout number="1">
          <staff-distance>72.00</staff-distance>
          </staff-layout>
        </print>
      <attributes>
        <divisions>16</divisions>
        <key>
          <fifths>0</fifths>
          </key>
        <time>
          <beats>3</beats>
          <beat-type>8</beat-type>
          </time>
        <clef>
          <sign>percussion</sign>
          <line>2</line>
          </clef>
        <staff-details>
          <staff-lines>1</staff-lines>
          </staff-details>
        </attributes>
      <direction placement="below">
        <direction-type>
          <dynamics default-x="3.25" default-y="-33.29" relative-y="-25.00">
            <mf/>
            </dynamics>
          </direction-type>
        <sound dynamics="88.89"/>
        </direction>
      <note default-x="69.00" default-y="-709.03">
        <unpitched>
          <display-step>E</display-step>
          <display-octave>4</display-octave>
          </unpitched>
        <duration>16</duration>
        <instrument id="P9-I82"/>
        <voice>1</voice>
        <type>quarter</type>
        <stem>down</stem>
        <notations>
          <ornaments>
            <trill-mark/>
            </ornaments>
          </notations>
        </note>
      <note>
        <rest/>
        <duration>8</duration>
        <voice>1</voice>
        <type>eighth</type>
        </note>
      </measure>
    <measure number="2" width="210.54">
      <note default-x="13.00" default-y="-709.03">
        <unpitched>
          <display-step>E</display-step>
          <display-octave>4</display-octave>
          </unpitched>
        <duration>16</duration>
        <instrument id="P9-I82"/>
        <voice>1</voice>
        <type>quarter</type>
        <stem>down</stem>
        <notations>
          <ornaments>
            <trill-mark/>
            </ornaments>
          </notations>
        </note>
      <note>
        <rest/>
        <duration>8</duration>
        <voice>1</voice>
        <type>eighth</type>
        </note>
      </measure>
    <measure number="3" width="210.54">
      <note default-x="13.00" default-y="-709.03">
        <unpitched>
          <display-step>E</display-step>
          <display-octave>4</display-octave>
          </unpitched>
        <duration>16</duration>
        <instrument id="P9-I82"/>
        <voice>1</voice>
        <type>quarter</type>
        <stem>down</stem>
        <notations>
          <ornaments>
            <trill-mark/>
            </ornaments>
          </notations>
        </note>
      <note>
        <rest/>
        <duration>8</duration>
        <voice>1</voice>
        <type>eighth</type>
        </note>
      </measure>
    <measure number="4" width="219.39">
      <note default-x="13.00" default-y="-709.03">
        <unpitched>
          <display-step>E</display-step>
          <display-octave>4</display-octave>
          </unpitched>
        <duration>16</duration>
        <instrument id="P9-I82"/>
        <voice>1</voice>
        <type>quarter</type>
        <stem>down</stem>
        <notations>
          <ornaments>
            <trill-mark/>
            </ornaments>
          </notations>
        </note>
      <note>
        <rest/>
        <duration>8</duration>
        <voice>1</voice>
        <type>eighth</type>
        </note>
      <barline location="right">
        <bar-style>light-heavy</bar-style>
        </barline>
      </measure>
    </part>
  <part id="P10">
    <measure number="1" width="266.54">
      <print>
        <staff-layout number="1">
          <staff-distance>44.72</staff-distance>
          </staff-layout>
        </print>
      <attributes>
        <divisions>16</divisions>
        <key>
          <fifths>0</fifths>
          </key>
        <time>
          <beats>3</beats>
          <beat-type>8</beat-type>
          </time>
        <clef>
          <sign>percussion</sign>
          <line>2</line>
          </clef>
        <staff-details>
          <staff-lines>1</staff-lines>
          </staff-details>
        </attributes>
      <direction placement="below">
        <direction-type>
          <dynamics default-x="3.25" default-y="-33.29" relative-y="-25.00">
            <mf/>
            </dynamics>
          </direction-type>
        <sound dynamics="88.89"/>
        </direction>
      <note>
        <rest/>
        <duration>8</duration>
        <voice>1</voice>
        <type>eighth</type>
        </note>
      <note>
        <rest/>
        <duration>8</duration>
        <voice>1</voice>
        <type>eighth</type>
        </note>
      <note>
        <rest/>
        <duration>8</duration>
        <voice>1</voice>
        <type>eighth</type>
        </note>
      </measure>
    <measure number="2" width="210.54">
      <note default-x="13.00" default-y="-793.75">
        <unpitched>
          <display-step>E</display-step>
          <display-octave>4</display-octave>
          </unpitched>
        <duration>24</duration>
        <instrument id="P10-I82"/>
        <voice>1</voice>
        <type>quarter</type>
        <dot/>
        <stem>down</stem>
        <notations>
          <ornaments>
            <trill-mark/>
            </ornaments>
          </notations>
        </note>
      </measure>
    <measure number="3" width="210.54">
      <note default-x="13.00" default-y="-793.75">
        <unpitched>
          <display-step>E</display-step>
          <display-octave>4</display-octave>
          </unpitched>
        <duration>24</duration>
        <instrument id="P10-I82"/>
        <voice>1</voice>
        <type>quarter</type>
        <dot/>
        <stem>down</stem>
        <notations>
          <ornaments>
            <trill-mark/>
            </ornaments>
          </notations>
        </note>
      </measure>
    <measure number="4" width="219.39">
      <note default-x="13.00" default-y="-793.75">
        <unpitched>
          <display-step>E</display-step>
          <display-octave>4</display-octave>
          </unpitched>
        <duration>24</duration>
        <instrument id="P10-I82"/>
        <voice>1</voice>
        <type>quarter</type>
        <dot/>
        <stem>down</stem>
        <notations>
          <ornaments>
            <trill-mark/>
            </ornaments>
          </notations>
        </note>
      <barline location="right">
        <bar-style>light-heavy</bar-style>
        </barline>
      </measure>
    </part>
  <part id="P11">
    <measure number="1" width="266.54">
      <print>
        <staff-layout number="1">
          <staff-distance>44.72</staff-distance>
          </staff-layout>
        </print>
      <attributes>
        <divisions>16</divisions>
        <key>
          <fifths>0</fifths>
          </key>
        <time>
          <beats>3</beats>
          <beat-type>8</beat-type>
          </time>
        <clef>
          <sign>percussion</sign>
          <line>2</line>
          </clef>
        <staff-details>
          <staff-lines>1</staff-lines>
          </staff-details>
        </attributes>
      <note default-x="69.00" default-y="-878.48">
        <unpitched>
          <display-step>E</display-step>
          <display-octave>4</display-octave>
          </unpitched>
        <duration>2</duration>
        <instrument id="P11-I81"/>
        <voice>1</voice>
        <type>32nd</type>
        <stem>down</stem>
        <notehead>x</notehead>
        <beam number="1">begin</beam>
        <beam number="2">begin</beam>
        <beam number="3">forward hook</beam>
        </note>
      <direction placement="below">
        <direction-type>
          <dynamics default-x="3.25" default-y="-31.29" relative-y="-25.00">
            <p/>
            </dynamics>
          </direction-type>
        <sound dynamics="54.44"/>
        </direction>
      <note default-x="98.99" default-y="-878.48">
        <unpitched>
          <display-step>E</display-step>
          <display-octave>4</display-octave>
          </unpitched>
        <duration>6</duration>
        <tie type="start"/>
        <instrument id="P11-I82"/>
        <voice>1</voice>
        <type>16th</type>
        <dot/>
        <stem>down</stem>
        <beam number="1">continue</beam>
        <beam number="2">continue</beam>
        <notations>
          <tied type="start"/>
          </notations>
        </note>
      <note default-x="131.82" default-y="-878.48">
        <unpitched>
          <display-step>E</display-step>
          <display-octave>4</display-octave>
          </unpitched>
        <duration>4</duration>
        <tie type="stop"/>
        <instrument id="P11-I82"/>
        <voice>1</voice>
        <type>16th</type>
        <stem>down</stem>
        <beam number="1">end</beam>
        <beam number="2">end</beam>
        <notations>
          <tied type="stop"/>
          </notations>
        </note>
      <note>
        <rest/>
        <duration>12</duration>
        <voice>1</voice>
        <type>eighth</type>
        <dot/>
        </note>
      </measure>
    <measure number="2" width="210.54">
      <note default-x="13.00" default-y="-878.48">
        <unpitched>
          <display-step>E</display-step>
          <display-octave>4</display-octave>
          </unpitched>
        <duration>2</duration>
        <instrument id="P11-I81"/>
        <voice>1</voice>
        <type>32nd</type>
        <stem>down</stem>
        <notehead>x</notehead>
        <beam number="1">begin</beam>
        <beam number="2">begin</beam>
        <beam number="3">forward hook</beam>
        </note>
      <direction placement="below">
        <direction-type>
          <dynamics default-x="3.25" default-y="-31.29" relative-y="-25.00">
            <p/>
            </dynamics>
          </direction-type>
        <sound dynamics="54.44"/>
        </direction>
      <note default-x="42.99" default-y="-878.48">
        <unpitched>
          <display-step>E</display-step>
          <display-octave>4</display-octave>
          </unpitched>
        <duration>6</duration>
        <tie type="start"/>
        <instrument id="P11-I82"/>
        <voice>1</voice>
        <type>16th</type>
        <dot/>
        <stem>down</stem>
        <beam number="1">continue</beam>
        <beam number="2">continue</beam>
        <notations>
          <tied type="start"/>
          </notations>
        </note>
      <note default-x="75.82" default-y="-878.48">
        <unpitched>
          <display-step>E</display-step>
          <display-octave>4</display-octave>
          </unpitched>
        <duration>4</duration>
        <tie type="stop"/>
        <instrument id="P11-I82"/>
        <voice>1</voice>
        <type>16th</type>
        <stem>down</stem>
        <beam number="1">end</beam>
        <beam number="2">end</beam>
        <notations>
          <tied type="stop"/>
          </notations>
        </note>
      <note>
        <rest/>
        <duration>12</duration>
        <voice>1</voice>
        <type>eighth</type>
        <dot/>
        </note>
      </measure>
    <measure number="3" width="210.54">
      <note default-x="13.00" default-y="-878.48">
        <unpitched>
          <display-step>E</display-step>
          <display-octave>4</display-octave>
          </unpitched>
        <duration>2</duration>
        <instrument id="P11-I81"/>
        <voice>1</voice>
        <type>32nd</type>
        <stem>down</stem>
        <notehead>x</notehead>
        <beam number="1">begin</beam>
        <beam number="2">begin</beam>
        <beam number="3">forward hook</beam>
        </note>
      <direction placement="below">
        <direction-type>
          <dynamics default-x="3.25" default-y="-31.29" relative-y="-25.00">
            <p/>
            </dynamics>
          </direction-type>
        <sound dynamics="54.44"/>
        </direction>
      <note default-x="42.99" default-y="-878.48">
        <unpitched>
          <display-step>E</display-step>
          <display-octave>4</display-octave>
          </unpitched>
        <duration>6</duration>
        <tie type="start"/>
        <instrument id="P11-I82"/>
        <voice>1</voice>
        <type>16th</type>
        <dot/>
        <stem>down</stem>
        <beam number="1">continue</beam>
        <beam number="2">continue</beam>
        <notations>
          <tied type="start"/>
          </notations>
        </note>
      <note default-x="75.82" default-y="-878.48">
        <unpitched>
          <display-step>E</display-step>
          <display-octave>4</display-octave>
          </unpitched>
        <duration>4</duration>
        <tie type="stop"/>
        <instrument id="P11-I82"/>
        <voice>1</voice>
        <type>16th</type>
        <stem>down</stem>
        <beam number="1">end</beam>
        <beam number="2">end</beam>
        <notations>
          <tied type="stop"/>
          </notations>
        </note>
      <note>
        <rest/>
        <duration>12</duration>
        <voice>1</voice>
        <type>eighth</type>
        <dot/>
        </note>
      </measure>
    <measure number="4" width="219.39">
      <note default-x="13.00" default-y="-878.48">
        <unpitched>
          <display-step>E</display-step>
          <display-octave>4</display-octave>
          </unpitched>
        <duration>2</duration>
        <instrument id="P11-I81"/>
        <voice>1</voice>
        <type>32nd</type>
        <stem>down</stem>
        <notehead>x</notehead>
        <beam number="1">begin</beam>
        <beam number="2">begin</beam>
        <beam number="3">forward hook</beam>
        </note>
      <direction placement="below">
        <direction-type>
          <dynamics default-x="3.25" default-y="-31.29" relative-y="-25.00">
            <p/>
            </dynamics>
          </direction-type>
        <sound dynamics="54.44"/>
        </direction>
      <note default-x="42.99" default-y="-878.48">
        <unpitched>
          <display-step>E</display-step>
          <display-octave>4</display-octave>
          </unpitched>
        <duration>6</duration>
        <tie type="start"/>
        <instrument id="P11-I82"/>
        <voice>1</voice>
        <type>16th</type>
        <dot/>
        <stem>down</stem>
        <beam number="1">continue</beam>
        <beam number="2">continue</beam>
        <notations>
          <tied type="start"/>
          </notations>
        </note>
      <note default-x="75.82" default-y="-878.48">
        <unpitched>
          <display-step>E</display-step>
          <display-octave>4</display-octave>
          </unpitched>
        <duration>4</duration>
        <tie type="stop"/>
        <instrument id="P11-I82"/>
        <voice>1</voice>
        <type>16th</type>
        <stem>down</stem>
        <beam number="1">end</beam>
        <beam number="2">end</beam>
        <notations>
          <tied type="stop"/>
          </notations>
        </note>
      <note>
        <rest/>
        <duration>12</duration>
        <voice>1</voice>
        <type>eighth</type>
        <dot/>
        </note>
      <barline location="right">
        <bar-style>light-heavy</bar-style>
        </barline>
      </measure>
    </part>
  <part id="P12">
    <measure number="1" width="266.54">
      <print>
        <staff-layout number="1">
          <staff-distance>44.72</staff-distance>
          </staff-layout>
        </print>
      <attributes>
        <divisions>16</divisions>
        <key>
          <fifths>0</fifths>
          </key>
        <time>
          <beats>3</beats>
          <beat-type>8</beat-type>
          </time>
        <clef>
          <sign>percussion</sign>
          <line>2</line>
          </clef>
        <staff-details>
          <staff-lines>1</staff-lines>
          </staff-details>
        </attributes>
      <note default-x="69.00" default-y="-963.20">
        <unpitched>
          <display-step>E</display-step>
          <display-octave>4</display-octave>
          </unpitched>
        <duration>1</duration>
        <instrument id="P12-I81"/>
        <voice>1</voice>
        <type>64th</type>
        <stem>down</stem>
        <notehead>x</notehead>
        <beam number="1">begin</beam>
        <beam number="2">begin</beam>
        <beam number="3">forward hook</beam>
        <beam number="4">forward hook</beam>
        </note>
      <direction placement="below">
        <direction-type>
          <dynamics default-x="3.25" default-y="-41.29" relative-y="-25.00">
            <mp/>
            </dynamics>
          </direction-type>
        <sound dynamics="71.11"/>
        </direction>
      <note default-x="84.00" default-y="-963.20">
        <unpitched>
          <display-step>E</display-step>
          <display-octave>4</display-octave>
          </unpitched>
        <duration>7</duration>
        <instrument id="P12-I82"/>
        <voice>1</voice>
        <type>16th</type>
        <dot/>
        <dot/>
        <stem>down</stem>
        <beam number="1">end</beam>
        <beam number="2">end</beam>
        </note>
      <note>
        <rest/>
        <duration>8</duration>
        <voice>1</voice>
        <type>eighth</type>
        </note>
      <note>
        <rest/>
        <duration>8</duration>
        <voice>1</voice>
        <type>eighth</type>
        </note>
      </measure>
    <measure number="2" width="210.54">
      <note default-x="13.00" default-y="-963.20">
        <unpitched>
          <display-step>E</display-step>
          <display-octave>4</display-octave>
          </unpitched>
        <duration>1</duration>
        <instrument id="P12-I81"/>
        <voice>1</voice>
        <type>64th</type>
        <stem>down</stem>
        <notehead>x</notehead>
        <beam number="1">begin</beam>
        <beam number="2">begin</beam>
        <beam number="3">forward hook</beam>
        <beam number="4">forward hook</beam>
        </note>
      <direction placement="below">
        <direction-type>
          <dynamics default-x="3.25" default-y="-41.29" relative-y="-25.00">
            <mp/>
            </dynamics>
          </direction-type>
        <sound dynamics="71.11"/>
        </direction>
      <note default-x="28.00" default-y="-963.20">
        <unpitched>
          <display-step>E</display-step>
          <display-octave>4</display-octave>
          </unpitched>
        <duration>7</duration>
        <instrument id="P12-I82"/>
        <voice>1</voice>
        <type>16th</type>
        <dot/>
        <dot/>
        <stem>down</stem>
        <beam number="1">end</beam>
        <beam number="2">end</beam>
        </note>
      <note>
        <rest/>
        <duration>8</duration>
        <voice>1</voice>
        <type>eighth</type>
        </note>
      <note>
        <rest/>
        <duration>8</duration>
        <voice>1</voice>
        <type>eighth</type>
        </note>
      </measure>
    <measure number="3" width="210.54">
      <note default-x="13.00" default-y="-963.20">
        <unpitched>
          <display-step>E</display-step>
          <display-octave>4</display-octave>
          </unpitched>
        <duration>1</duration>
        <instrument id="P12-I81"/>
        <voice>1</voice>
        <type>64th</type>
        <stem>down</stem>
        <notehead>x</notehead>
        <beam number="1">begin</beam>
        <beam number="2">begin</beam>
        <beam number="3">forward hook</beam>
        <beam number="4">forward hook</beam>
        </note>
      <direction placement="below">
        <direction-type>
          <dynamics default-x="3.25" default-y="-41.29" relative-y="-25.00">
            <mp/>
            </dynamics>
          </direction-type>
        <sound dynamics="71.11"/>
        </direction>
      <note default-x="28.00" default-y="-963.20">
        <unpitched>
          <display-step>E</display-step>
          <display-octave>4</display-octave>
          </unpitched>
        <duration>7</duration>
        <instrument id="P12-I82"/>
        <voice>1</voice>
        <type>16th</type>
        <dot/>
        <dot/>
        <stem>down</stem>
        <beam number="1">end</beam>
        <beam number="2">end</beam>
        </note>
      <note>
        <rest/>
        <duration>8</duration>
        <voice>1</voice>
        <type>eighth</type>
        </note>
      <note>
        <rest/>
        <duration>8</duration>
        <voice>1</voice>
        <type>eighth</type>
        </note>
      </measure>
    <measure number="4" width="219.39">
      <note default-x="13.00" default-y="-963.20">
        <unpitched>
          <display-step>E</display-step>
          <display-octave>4</display-octave>
          </unpitched>
        <duration>1</duration>
        <instrument id="P12-I81"/>
        <voice>1</voice>
        <type>64th</type>
        <stem>down</stem>
        <notehead>x</notehead>
        <beam number="1">begin</beam>
        <beam number="2">begin</beam>
        <beam number="3">forward hook</beam>
        <beam number="4">forward hook</beam>
        </note>
      <direction placement="below">
        <direction-type>
          <dynamics default-x="3.25" default-y="-41.29" relative-y="-25.00">
            <mp/>
            </dynamics>
          </direction-type>
        <sound dynamics="71.11"/>
        </direction>
      <note default-x="28.00" default-y="-963.20">
        <unpitched>
          <display-step>E</display-step>
          <display-octave>4</display-octave>
          </unpitched>
        <duration>7</duration>
        <instrument id="P12-I82"/>
        <voice>1</voice>
        <type>16th</type>
        <dot/>
        <dot/>
        <stem>down</stem>
        <beam number="1">end</beam>
        <beam number="2">end</beam>
        </note>
      <note>
        <rest/>
        <duration>8</duration>
        <voice>1</voice>
        <type>eighth</type>
        </note>
      <note>
        <rest/>
        <duration>8</duration>
        <voice>1</voice>
        <type>eighth</type>
        </note>
      <barline location="right">
        <bar-style>light-heavy</bar-style>
        </barline>
      </measure>
    </part>
  <part id="P13">
    <measure number="1" width="266.54">
      <print>
        <staff-layout number="1">
          <staff-distance>48.70</staff-distance>
          </staff-layout>
        </print>
      <attributes>
        <divisions>16</divisions>
        <key>
          <fifths>0</fifths>
          </key>
        <time>
          <beats>3</beats>
          <beat-type>8</beat-type>
          </time>
        <clef>
          <sign>percussion</sign>
          <line>2</line>
          </clef>
        <staff-details>
          <staff-lines>1</staff-lines>
          </staff-details>
        </attributes>
      <direction placement="below">
        <direction-type>
          <dynamics default-x="3.25" default-y="-33.29" relative-y="-25.00">
            <mf/>
            </dynamics>
          </direction-type>
        <sound dynamics="88.89"/>
        </direction>
      <note default-x="69.00" default-y="-1051.90">
        <unpitched>
          <display-step>E</display-step>
          <display-octave>4</display-octave>
          </unpitched>
        <duration>12</duration>
        <instrument id="P13-I82"/>
        <voice>1</voice>
        <type>eighth</type>
        <dot/>
        <stem>down</stem>
        </note>
      <note>
        <rest/>
        <duration>12</duration>
        <voice>1</voice>
        <type>eighth</type>
        <dot/>
        </note>
      </measure>
    <measure number="2" width="210.54">
      <direction placement="below">
        <direction-type>
          <dynamics default-x="3.25" default-y="-33.29" relative-y="-25.00">
            <mf/>
            </dynamics>
          </direction-type>
        <sound dynamics="88.89"/>
        </direction>
      <note default-x="13.00" default-y="-1051.90">
        <unpitched>
          <display-step>E</display-step>
          <display-octave>4</display-octave>
          </unpitched>
        <duration>12</duration>
        <instrument id="P13-I82"/>
        <voice>1</voice>
        <type>eighth</type>
        <dot/>
        <stem>down</stem>
        </note>
      <note>
        <rest/>
        <duration>12</duration>
        <voice>1</voice>
        <type>eighth</type>
        <dot/>
        </note>
      </measure>
    <measure number="3" width="210.54">
      <direction placement="below">
        <direction-type>
          <dynamics default-x="3.25" default-y="-33.29" relative-y="-25.00">
            <mf/>
            </dynamics>
          </direction-type>
        <sound dynamics="88.89"/>
        </direction>
      <note default-x="13.00" default-y="-1051.90">
        <unpitched>
          <display-step>E</display-step>
          <display-octave>4</display-octave>
          </unpitched>
        <duration>12</duration>
        <instrument id="P13-I82"/>
        <voice>1</voice>
        <type>eighth</type>
        <dot/>
        <stem>down</stem>
        </note>
      <note>
        <rest/>
        <duration>12</duration>
        <voice>1</voice>
        <type>eighth</type>
        <dot/>
        </note>
      </measure>
    <measure number="4" width="219.39">
      <direction placement="below">
        <direction-type>
          <dynamics default-x="3.25" default-y="-33.29" relative-y="-25.00">
            <mf/>
            </dynamics>
          </direction-type>
        <sound dynamics="88.89"/>
        </direction>
      <note default-x="13.00" default-y="-1051.90">
        <unpitched>
          <display-step>E</display-step>
          <display-octave>4</display-octave>
          </unpitched>
        <duration>12</duration>
        <instrument id="P13-I82"/>
        <voice>1</voice>
        <type>eighth</type>
        <dot/>
        <stem>down</stem>
        </note>
      <note>
        <rest/>
        <duration>12</duration>
        <voice>1</voice>
        <type>eighth</type>
        <dot/>
        </note>
      <barline location="right">
        <bar-style>light-heavy</bar-style>
        </barline>
      </measure>
    </part>
  <part id="P14">
    <measure number="1" width="266.54">
      <print>
        <staff-layout number="1">
          <staff-distance>44.72</staff-distance>
          </staff-layout>
        </print>
      <attributes>
        <divisions>16</divisions>
        <key>
          <fifths>0</fifths>
          </key>
        <time>
          <beats>3</beats>
          <beat-type>8</beat-type>
          </time>
        <clef>
          <sign>percussion</sign>
          <line>2</line>
          </clef>
        <staff-details>
          <staff-lines>1</staff-lines>
          </staff-details>
        </attributes>
      <direction placement="below">
        <direction-type>
          <dynamics default-x="3.25" default-y="-33.29" relative-y="-25.00">
            <mf/>
            </dynamics>
          </direction-type>
        <sound dynamics="88.89"/>
        </direction>
      <note default-x="69.00" default-y="-1136.62">
        <unpitched>
          <display-step>E</display-step>
          <display-octave>4</display-octave>
          </unpitched>
        <duration>16</duration>
        <instrument id="P14-I81"/>
        <voice>1</voice>
        <type>quarter</type>
        <stem>down</stem>
        <notehead>x</notehead>
        </note>
      <note default-x="201.30" default-y="-1136.62">
        <unpitched>
          <display-step>E</display-step>
          <display-octave>4</display-octave>
          </unpitched>
        <duration>6</duration>
        <instrument id="P14-I81"/>
        <voice>1</voice>
        <type>16th</type>
        <dot/>
        <stem>down</stem>
        <notehead>x</notehead>
        <beam number="1">begin</beam>
        <beam number="2">begin</beam>
        </note>
      <note default-x="234.12" default-y="-1136.62">
        <unpitched>
          <display-step>E</display-step>
          <display-octave>4</display-octave>
          </unpitched>
        <duration>2</duration>
        <instrument id="P14-I82"/>
        <voice>1</voice>
        <type>32nd</type>
        <stem>down</stem>
        <beam number="1">end</beam>
        <beam number="2">end</beam>
        <beam number="3">backward hook</beam>
        </note>
      </measure>
    <measure number="2" width="210.54">
      <note default-x="13.00" default-y="-1136.62">
        <unpitched>
          <display-step>E</display-step>
          <display-octave>4</display-octave>
          </unpitched>
        <duration>16</duration>
        <instrument id="P14-I81"/>
        <voice>1</voice>
        <type>quarter</type>
        <stem>down</stem>
        <notehead>x</notehead>
        </note>
      <note default-x="145.30" default-y="-1136.62">
        <unpitched>
          <display-step>E</display-step>
          <display-octave>4</display-octave>
          </unpitched>
        <duration>6</duration>
        <instrument id="P14-I81"/>
        <voice>1</voice>
        <type>16th</type>
        <dot/>
        <stem>down</stem>
        <notehead>x</notehead>
        <beam number="1">begin</beam>
        <beam number="2">begin</beam>
        </note>
      <note default-x="178.12" default-y="-1136.62">
        <unpitched>
          <display-step>E</display-step>
          <display-octave>4</display-octave>
          </unpitched>
        <duration>2</duration>
        <instrument id="P14-I82"/>
        <voice>1</voice>
        <type>32nd</type>
        <stem>down</stem>
        <beam number="1">end</beam>
        <beam number="2">end</beam>
        <beam number="3">backward hook</beam>
        </note>
      </measure>
    <measure number="3" width="210.54">
      <note default-x="13.00" default-y="-1136.62">
        <unpitched>
          <display-step>E</display-step>
          <display-octave>4</display-octave>
          </unpitched>
        <duration>16</duration>
        <instrument id="P14-I81"/>
        <voice>1</voice>
        <type>quarter</type>
        <stem>down</stem>
        <notehead>x</notehead>
        </note>
      <note default-x="145.30" default-y="-1136.62">
        <unpitched>
          <display-step>E</display-step>
          <display-octave>4</display-octave>
          </unpitched>
        <duration>6</duration>
        <instrument id="P14-I81"/>
        <voice>1</voice>
        <type>16th</type>
        <dot/>
        <stem>down</stem>
        <notehead>x</notehead>
        <beam number="1">begin</beam>
        <beam number="2">begin</beam>
        </note>
      <note default-x="178.12" default-y="-1136.62">
        <unpitched>
          <display-step>E</display-step>
          <display-octave>4</display-octave>
          </unpitched>
        <duration>2</duration>
        <instrument id="P14-I82"/>
        <voice>1</voice>
        <type>32nd</type>
        <stem>down</stem>
        <beam number="1">end</beam>
        <beam number="2">end</beam>
        <beam number="3">backward hook</beam>
        </note>
      </measure>
    <measure number="4" width="219.39">
      <note default-x="13.00" default-y="-1136.62">
        <unpitched>
          <display-step>E</display-step>
          <display-octave>4</display-octave>
          </unpitched>
        <duration>16</duration>
        <instrument id="P14-I81"/>
        <voice>1</voice>
        <type>quarter</type>
        <stem>down</stem>
        <notehead>x</notehead>
        </note>
      <note default-x="145.30" default-y="-1136.62">
        <unpitched>
          <display-step>E</display-step>
          <display-octave>4</display-octave>
          </unpitched>
        <duration>6</duration>
        <instrument id="P14-I81"/>
        <voice>1</voice>
        <type>16th</type>
        <dot/>
        <stem>down</stem>
        <notehead>x</notehead>
        <beam number="1">begin</beam>
        <beam number="2">begin</beam>
        </note>
      <note default-x="178.12" default-y="-1136.62">
        <unpitched>
          <display-step>E</display-step>
          <display-octave>4</display-octave>
          </unpitched>
        <duration>2</duration>
        <instrument id="P14-I82"/>
        <voice>1</voice>
        <type>32nd</type>
        <stem>down</stem>
        <beam number="1">end</beam>
        <beam number="2">end</beam>
        <beam number="3">backward hook</beam>
        </note>
      <barline location="right">
        <bar-style>light-heavy</bar-style>
        </barline>
      </measure>
    </part>
  <part id="P15">
    <measure number="1" width="266.54">
      <print>
        <staff-layout number="1">
          <staff-distance>44.72</staff-distance>
          </staff-layout>
        </print>
      <attributes>
        <divisions>16</divisions>
        <key>
          <fifths>0</fifths>
          </key>
        <time>
          <beats>3</beats>
          <beat-type>8</beat-type>
          </time>
        <clef>
          <sign>percussion</sign>
          <line>2</line>
          </clef>
        <staff-details>
          <staff-lines>1</staff-lines>
          </staff-details>
        </attributes>
      <direction placement="below">
        <direction-type>
          <dynamics default-x="3.25" default-y="-33.29" relative-y="-25.00">
            <mf/>
            </dynamics>
          </direction-type>
        <sound dynamics="88.89"/>
        </direction>
      <note default-x="69.00" default-y="-1221.34">
        <unpitched>
          <display-step>E</display-step>
          <display-octave>4</display-octave>
          </unpitched>
        <duration>16</duration>
        <instrument id="P15-I81"/>
        <voice>1</voice>
        <type>quarter</type>
        <stem>down</stem>
        <notehead>x</notehead>
        </note>
      <note default-x="201.30" default-y="-1221.34">
        <unpitched>
          <display-step>E</display-step>
          <display-octave>4</display-octave>
          </unpitched>
        <duration>6</duration>
        <instrument id="P15-I81"/>
        <voice>1</voice>
        <type>16th</type>
        <dot/>
        <stem>down</stem>
        <notehead>x</notehead>
        <beam number="1">begin</beam>
        <beam number="2">begin</beam>
        </note>
      <note default-x="234.12" default-y="-1221.34">
        <unpitched>
          <display-step>E</display-step>
          <display-octave>4</display-octave>
          </unpitched>
        <duration>2</duration>
        <instrument id="P15-I82"/>
        <voice>1</voice>
        <type>32nd</type>
        <stem>down</stem>
        <beam number="1">end</beam>
        <beam number="2">end</beam>
        <beam number="3">backward hook</beam>
        </note>
      </measure>
    <measure number="2" width="210.54">
      <note default-x="13.00" default-y="-1221.34">
        <unpitched>
          <display-step>E</display-step>
          <display-octave>4</display-octave>
          </unpitched>
        <duration>16</duration>
        <instrument id="P15-I81"/>
        <voice>1</voice>
        <type>quarter</type>
        <stem>down</stem>
        <notehead>x</notehead>
        </note>
      <note default-x="145.30" default-y="-1221.34">
        <unpitched>
          <display-step>E</display-step>
          <display-octave>4</display-octave>
          </unpitched>
        <duration>6</duration>
        <instrument id="P15-I81"/>
        <voice>1</voice>
        <type>16th</type>
        <dot/>
        <stem>down</stem>
        <notehead>x</notehead>
        <beam number="1">begin</beam>
        <beam number="2">begin</beam>
        </note>
      <note default-x="178.12" default-y="-1221.34">
        <unpitched>
          <display-step>E</display-step>
          <display-octave>4</display-octave>
          </unpitched>
        <duration>2</duration>
        <instrument id="P15-I82"/>
        <voice>1</voice>
        <type>32nd</type>
        <stem>down</stem>
        <beam number="1">end</beam>
        <beam number="2">end</beam>
        <beam number="3">backward hook</beam>
        </note>
      </measure>
    <measure number="3" width="210.54">
      <note default-x="13.00" default-y="-1221.34">
        <unpitched>
          <display-step>E</display-step>
          <display-octave>4</display-octave>
          </unpitched>
        <duration>16</duration>
        <instrument id="P15-I81"/>
        <voice>1</voice>
        <type>quarter</type>
        <stem>down</stem>
        <notehead>x</notehead>
        </note>
      <note default-x="145.30" default-y="-1221.34">
        <unpitched>
          <display-step>E</display-step>
          <display-octave>4</display-octave>
          </unpitched>
        <duration>6</duration>
        <instrument id="P15-I81"/>
        <voice>1</voice>
        <type>16th</type>
        <dot/>
        <stem>down</stem>
        <notehead>x</notehead>
        <beam number="1">begin</beam>
        <beam number="2">begin</beam>
        </note>
      <note default-x="178.12" default-y="-1221.34">
        <unpitched>
          <display-step>E</display-step>
          <display-octave>4</display-octave>
          </unpitched>
        <duration>2</duration>
        <instrument id="P15-I82"/>
        <voice>1</voice>
        <type>32nd</type>
        <stem>down</stem>
        <beam number="1">end</beam>
        <beam number="2">end</beam>
        <beam number="3">backward hook</beam>
        </note>
      </measure>
    <measure number="4" width="219.39">
      <note default-x="13.00" default-y="-1221.34">
        <unpitched>
          <display-step>E</display-step>
          <display-octave>4</display-octave>
          </unpitched>
        <duration>16</duration>
        <instrument id="P15-I81"/>
        <voice>1</voice>
        <type>quarter</type>
        <stem>down</stem>
        <notehead>x</notehead>
        </note>
      <note default-x="145.30" default-y="-1221.34">
        <unpitched>
          <display-step>E</display-step>
          <display-octave>4</display-octave>
          </unpitched>
        <duration>6</duration>
        <instrument id="P15-I81"/>
        <voice>1</voice>
        <type>16th</type>
        <dot/>
        <stem>down</stem>
        <notehead>x</notehead>
        <beam number="1">begin</beam>
        <beam number="2">begin</beam>
        </note>
      <note default-x="178.12" default-y="-1221.34">
        <unpitched>
          <display-step>E</display-step>
          <display-octave>4</display-octave>
          </unpitched>
        <duration>2</duration>
        <instrument id="P15-I82"/>
        <voice>1</voice>
        <type>32nd</type>
        <stem>down</stem>
        <beam number="1">end</beam>
        <beam number="2">end</beam>
        <beam number="3">backward hook</beam>
        </note>
      <barline location="right">
        <bar-style>light-heavy</bar-style>
        </barline>
      </measure>
    </part>
  </score-partwise>
"""
