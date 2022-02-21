module SequencerTests exposing (..)

import Array
import Composer.Sequencer exposing (transformSequenceToCommands)
import Dict
import Expect
import Extra.TypedTime as TypedTime
import FlowIO exposing (defaultCommand, defaultDevice)
import Messages exposing (CommandsEntry, Instructions)
import Set
import Test exposing (..)


fakeDevice1 : FlowIO.Device
fakeDevice1 =
    { defaultDevice
        | details =
            Just
                { name = "Fake"
                , id = fakeDeviceId1
                , services = []
                }
    }


fakeDevice2 : FlowIO.Device
fakeDevice2 =
    { defaultDevice
        | details =
            Just
                { name = "Faker"
                , id = fakeDeviceId2
                , services = []
                }
    }


fakeRole1 =
    "role 1"


fakeRole2 =
    "role 2"


fakeDeviceId1 =
    "fake id 1"


fakeDeviceId2 =
    "fake id 2"


transformSequenceTests : Test
transformSequenceTests =
    describe "transform sequence"
        [ test "simple two parts" <|
            \() ->
                let
                    instructions1 : Instructions
                    instructions1 =
                        { time = Array.fromList [ TypedTime.milliseconds 0, TypedTime.milliseconds 100 ]
                        , instructions =
                            Dict.fromList
                                [ ( fakeRole1
                                  , Array.fromList
                                        [ defaultCommand
                                        , { defaultCommand | action = FlowIO.Stop }
                                        ]
                                  )
                                ]
                        }

                    sequence : List ( String, Instructions )
                    sequence =
                        [ ( "1", instructions1 )
                        , ( "2", instructions1 )
                        ]

                    roles =
                        Dict.fromList [ ( fakeRole1, Set.fromList [ fakeDeviceId1 ] ) ]

                    devices =
                        Array.fromList [ fakeDevice1 ]

                    expected : List CommandsEntry
                    expected =
                        [ { startTime = TypedTime.milliseconds 0
                          , commands = [ { device = fakeDevice1, deviceIndex = 0, command = defaultCommand } ]
                          }
                          -- Duplicate instruction should be skipped
                        --, { startTime = TypedTime.milliseconds 100
                        --  , commands = [ { device = fakeDevice1, deviceIndex = 0, command = { defaultCommand | action = FlowIO.Stop } } ]
                        --  }
                        , { startTime = TypedTime.milliseconds 100
                          , commands = [ { device = fakeDevice1, deviceIndex = 0, command = defaultCommand } ]
                          }
                        , { startTime = TypedTime.milliseconds 200
                          , commands = [ { device = fakeDevice1, deviceIndex = 0, command = { defaultCommand | action = FlowIO.Stop } } ]
                          }
                        ]
                in
                transformSequenceToCommands { sequence = sequence, roleAssignments = roles, availableDevices = devices }
                    |> Expect.equalLists expected
        , test "two parts two roles" <|
            \() ->
                let
                    instructions1 : Instructions
                    instructions1 =
                        { time = Array.fromList [ TypedTime.milliseconds 0, TypedTime.milliseconds 100 ]
                        , instructions =
                            Dict.fromList
                                [ ( fakeRole1
                                  , Array.fromList
                                        [ defaultCommand
                                        , { defaultCommand | action = FlowIO.Stop }
                                        ]
                                  )
                                , ( fakeRole2
                                  , Array.fromList
                                        [ defaultCommand
                                        , { defaultCommand | action = FlowIO.Stop }
                                        ]
                                  )
                                ]
                        }

                    sequence : List ( String, Instructions )
                    sequence =
                        [ ( "1", instructions1 )
                        , ( "2", instructions1 )
                        ]

                    roles =
                        Dict.fromList
                            [ ( fakeRole1, Set.fromList [ fakeDeviceId1 ] )
                            , ( fakeRole2, Set.fromList [ fakeDeviceId2 ] )
                            ]

                    devices =
                        Array.fromList [ fakeDevice1, fakeDevice2 ]

                    expected : List CommandsEntry
                    expected =
                        [ { startTime = TypedTime.milliseconds 0
                          , commands =
                                [ { device = fakeDevice1, deviceIndex = 0, command = defaultCommand }
                                , { device = fakeDevice2, deviceIndex = 1, command = defaultCommand }
                                ]
                          }
                        -- Duplicate command should be skipped
                        --, { startTime = TypedTime.milliseconds 100
                        --  , commands =
                        --        [ { device = fakeDevice1, deviceIndex = 0, command = { defaultCommand | action = FlowIO.Stop } }
                        --        , { device = fakeDevice2, deviceIndex = 1, command = { defaultCommand | action = FlowIO.Stop } }
                        --        ]
                        --  }
                        , { startTime = TypedTime.milliseconds 100
                          , commands =
                                [ { device = fakeDevice1, deviceIndex = 0, command = defaultCommand }
                                , { device = fakeDevice2, deviceIndex = 1, command = defaultCommand }
                                ]
                          }
                        , { startTime = TypedTime.milliseconds 200
                          , commands =
                                [ { device = fakeDevice1, deviceIndex = 0, command = { defaultCommand | action = FlowIO.Stop } }
                                , { device = fakeDevice2, deviceIndex = 1, command = { defaultCommand | action = FlowIO.Stop } }
                                ]
                          }
                        ]
                in
                transformSequenceToCommands { sequence = sequence, roleAssignments = roles, availableDevices = devices }
                    |> Expect.equalLists expected
        ]
