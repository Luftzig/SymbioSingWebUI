module Extra.RemoteService exposing (RemoteService, init, initFromCommand, updateCommand, updateData, getData, getCommand)

{- This is modeled after the RemoteData type.
   A service is a remote provider that can receive commands and return data.
   It is basically the RemoteData model but bi-directional
-}


type RemoteService data command
    = Uninitialised
    | DataReceived data
    | CommandSent command
    | HasDataCommandSent data command


init : RemoteService data command
init =
    Uninitialised


initFromData : data -> RemoteService data command
initFromData data =
    DataReceived data


initFromCommand : command -> RemoteService data command
initFromCommand command =
    CommandSent command


updateData : data -> RemoteService data command -> RemoteService data command
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


updateCommand : cmd -> RemoteService data cmd -> RemoteService data cmd
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


getCommand : RemoteService data cmd -> Maybe cmd
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


getData : RemoteService data cmd -> Maybe data
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
