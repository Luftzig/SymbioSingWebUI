module Extra.RemoteService exposing (Service, init, initFromCommand, updateCommand, updateData, getData, getCommand)

{- This is modeled after the RemoteData type.
   A service is a remote provider that can receive commands and return data.
   It is basically the RemoteData model but bi-directional
-}


type Service data command
    = Uninitialised
    | DataReceived data
    | CommandSent command
    | HasDataCommandSent data command


init : Service data command
init =
    Uninitialised


initFromData : data -> Service data command
initFromData data =
    DataReceived data


initFromCommand : command -> Service data command
initFromCommand command =
    CommandSent command


updateData : data -> Service data command -> Service data command
updateData data service =
    case service of
        Uninitialised ->
            DataReceived data

        DataReceived _ ->
            DataReceived data

        CommandSent command ->
            HasDataCommandSent data command

        HasDataCommandSent _ command ->
            HasDataCommandSent data command


updateCommand : cmd -> Service data cmd -> Service data cmd
updateCommand cmd service =
    case service of
        Uninitialised ->
            CommandSent cmd

        DataReceived data ->
            HasDataCommandSent data cmd

        CommandSent _ ->
            CommandSent cmd

        HasDataCommandSent data _ ->
            HasDataCommandSent data cmd


getCommand : Service data cmd -> Maybe cmd
getCommand service =
    case service of
        Uninitialised ->
            Nothing

        DataReceived data ->
            Nothing

        CommandSent command ->
            Just command

        HasDataCommandSent data command ->
            Just command


getData : Service data cmd -> Maybe data
getData service =
    case service of
        Uninitialised ->
            Nothing

        DataReceived data ->
            Just data

        CommandSent command ->
            Nothing

        HasDataCommandSent data command ->
            Just data
