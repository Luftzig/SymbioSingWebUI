module Messages exposing (..)

import Array exposing (Array)
import Dict exposing (Dict)
import Extra.Resource exposing (Resource)
import Extra.TypedTime exposing (TypedTime)
import File exposing (File)
import FlowIO exposing (Action, AnalogReadings, AnalogServiceRequest, Command, Configuration, Device, DeviceId, Port, PowerOffStatus, Service)
import Json.Decode exposing (Value)
import PeerSync exposing (PeerSyncCommand, PeerSyncMessage)
import Task
import Time exposing (Posix)


type MainTab
    = SchedulerTab
    | SensorReadingsTab
    | NotationConverterTab
    | SequencerTab


type Msg
    = AddDevice
    | RemoveDevice Int
    | ConnectToDevice Int
    | DeviceStatusChanged { deviceIndex : Int, status : String, details : Maybe Value }
    | RequestControlServiceUpdates Int
    | DisconnectDevice Int
    | ControlServiceUpdate { deviceIndex : Int, status : Value }
    | SendCommand Int Command
    | ChangeCommandPortState Int FlowIO.Port FlowIO.PortState
    | ChangeCommandPwm Int Int
    | ActionClicked Int Action
    | ActionReleased
    | SchedulerMessage SchedulerMsg
    | DeviceConfigurationChanged { deviceIndex : Int, configuration : Maybe Configuration }
    | RequestDeviceConfiguration Int
    | SetDeviceConfiguration Int Configuration
    | ToggleServicePanelState
    | AddServiceToPanel Int Service
    | RemoveServiceFromPanel Int Service
    | DevicePowerOffStatusChange Int PowerOffStatus
    | SendNewPowerOffStatus Int PowerOffStatus
    | SensorReadingReceived Int (Result Json.Decode.Error AnalogReadings)
    | SensorReadingTimestampAttached Int ( Time.Posix, AnalogReadings )
    | SensorReadingModeChanged Int AnalogServiceRequest
    | BatteryReadingRequested Int
    | BatteryReadingReceived { deviceIndex : Int, level : Float }
    | SensorsMessage SensorsMsg
    | ComposerMessage ConverterMsg
    | ChangeTabTo MainTab
    | WindowDimensionsChanged Int Int
    | NoAction String
    | ToggleErrorLog
    | ScheduleLoaded String String
    | ReceivedSavedSchedules (List String)
    | ToggleSavedMenu
    | SavedScheduleRequested String
    | SequencerMessage SequencerMsg
    | SendInstructionsToSequencerRequestedFromScheduler
    | SendInstructionsToSequencerRequestedFromConverter
    | DialogBackDropClicked
    | PeerNameChanged String
    | SendPeerCommand PeerSyncCommand
    | PeerMessageReceived (Result Json.Decode.Error PeerSyncMessage)
    | CountdownRequested Float



-- Scheduler module


type alias RoleName =
    String


type RoleDeviceSelectState
    = SelectionClosed
    | SelectionOpen Int


type alias RolesInstructions =
    Dict RoleName (Array Command)


type alias Instructions =
    { time : Array TypedTime
    , instructions : RolesInstructions
    }


type SchedulerIncomingMsg
    = InstructionsLoaded String Instructions
    | ConverterScheduleUpdates (Resource String Instructions)


type SchedulerMsg
    = AddInstruction
    | DeleteLastInstruction
    | ResetInstructions
    | InstructionTimeChanged Int String
    | ActionChanged RoleName Int Action
    | PWMChanged RoleName Int String
    | PortStateChanged RoleName Int Port Bool
    | RunInstructions
    | StartInstructions Posix
    | StopInstructions
    | DownloadInstructions
    | SaveInstructions
    | UploadInstructions
    | UploadSelected File.File
    | FileRead String
    | DisabledFieldClicked String
    | AddRole String
    | RemoveRole Int
    | ReorderRole Int Int
    | RenameRole Int String
    | EditRole Int
    | EndRoleEditing
    | AssociateRoleToDevice RoleName (Maybe String)
    | ChangeDeviceSelection RoleDeviceSelectState
    | SchedulerTick Posix
    | LoadFromConverter
    | ReceivedIncomingMsg SchedulerIncomingMsg
    | ScheduleNameChanged String



-- Sensors module


type SensorsMsg
    = NewReading DeviceId Time.Posix AnalogReadings
    | DeviceSelected (Maybe DeviceId)



-- Converter module


type DynamicsSettings
    = PWMValues
    | RegulatorValues


type TrillInterval
    = Absolute TypedTime
    | PerBeat Float


type ConverterMsg
    = SelectScoreFile
    | FileSelected File
    | ParsingCompleted (Result String HapticScore)
    | PortSelected PartID FlowIO.Port
    | PartRoleNameChanged PartID String
    | RoleNameInputFieldFocused PartID
    | RoleNameInputFieldLostFocus PartID
    | LoseFocusDelayWaited PartID
    | RoleSuggestionSelected PartID String
    | ConversionRequested
    | DownloadRequested Instructions
    | BpmChanged String
    | UpdateDynamic Dynamic Float
    | UpdateRegulator Dynamic Float
    | NameChanged String
    | SaveSchedule String Instructions
    | ChangeSettingsTo DynamicsSettings
    | TrillIntervalChanged TrillInterval



-- Sequencer Module


type alias CommandsEntry =
    { startTime : TypedTime
    , commands : List { device : Device, deviceIndex : Int, command : Command }
    }


type MoveDirection
    = MoveUp
    | MoveDown


type SequencerIncomingMsg
    = DevicesChanged (Array Device)
    | ReceivedNewPart String Instructions
    | BackdropClicked


type SequencerMsg
    = ReceivedMessage SequencerIncomingMsg
    | RequestLoadFromScheduler
    | RequestFromConverter
    | LoadFromStorageRequested
    | LoadFromStorage String
    | RequestUploadInstructions
    | InstructionsFileSelected File
    | InstructionFileRead String String
    | OpenSaveSequenceDialog
    | DownloadSequence
    | RequestSequenceUpload
    | CloseDialog
    | AddPartToSequence String
    | TogglePartDetails String
    | RemoveFromSequence Int
    | MovePart Int MoveDirection
    | RoleClicked String
    | DeviceAssignmentChanged String Device Bool
    | PlaySequenceRequested
    | PlaySequenceStarted (List CommandsEntry) Posix
    | PlaySequenceStopped
    | SequencerTick Posix
    | CountdownReceived PeerSync.CountdownData
    | CountdownStartRequested Float



-- Composer.Notation


type alias HapticScore =
    Dict PartID HapticPart


type alias PartID =
    String


type alias HapticPart =
    { name : String
    , measures : List Measure
    }


type alias Measure =
    { number : Int
    , signature : Signature
    , divisionsPerQuarter : Int
    , notes : List HapticNote
    }


type alias PartialMeasure =
    { number : Int
    , signature : Maybe Signature
    , divisionsPerQuarter : Maybe Int
    , notes : List HapticNote
    }


type alias Signature =
    { beats : Int
    , beatType : Timing
    }


type HapticNote
    = Rest Dynamic Timing
    | Hold Dynamic Timing
    | Actuate Dynamic Timing
    | Trill Dynamic Timing -- Used only for the palm


type alias Timing =
    Int


type Dynamic
    = Pianississimo
    | Pianissimo
    | Piano
    | Mezzopiano
    | Mezzoforte
    | Forte
    | Fortissimo
    | Fortississimo


sendMessage : msg -> Cmd msg
sendMessage msg =
    Task.perform (\() -> msg) <|
        Task.succeed ()
