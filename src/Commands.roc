module [
    routeCommands
]

import pf.Cmd
import Storage exposing [FileAndTag]

routeCommands = \args, gitEnv, savedFileTags  ->
    rest = List.dropFirst args 1
    first = List.get args 0
        |> Result.withDefault ""
    filepathsByTag = indexFileTags savedFileTags
    when args is
        ["add", ..] -> addCommand rest gitEnv filepathsByTag
        ["subtract", ..] -> subtractCommand rest gitEnv filepathsByTag
        _ -> Task.err (CmdError (Str.concat "Unrecognized command: " first))

indexFileTags : List FileAndTag -> Dict Str Str
indexFileTags = \savedFileTags ->
    List.walk savedFileTags (Dict.empty {}) \byTag, {filepath, tag} ->
        newTags = Dict.insert byTag tag filepath
        newTags

tagsToFilepaths : Dict Str Str, List Str -> [Filepaths (List Str), Unrecognized (List Str)]
tagsToFilepaths = \filepathsByTag, names ->
    inspected = List.walk names { unrecognized: [], filepaths: [] } \state, name ->
        when Dict.get filepathsByTag name is
            Ok filepath -> { state & filepaths: List.append state.filepaths filepath }
            Err KeyNotFound -> { state & unrecognized: List.append state.unrecognized name }
    if List.isEmpty inspected.unrecognized then
        Filepaths inspected.filepaths
    else
        Unrecognized inspected.unrecognized



addCommand = \names, gitEnv, filepathsByTag ->
    when tagsToFilepaths filepathsByTag names is
        Filepaths filepaths ->
            args = List.prepend filepaths "add"
            Cmd.new gitEnv.gitBin
                |> Cmd.args args
                |> Cmd.status
                |> Task.mapErr \_ -> CmdError "Error executing git add command"
        Unrecognized unrecognizedNames ->
            namesStr = Str.joinWith unrecognizedNames ", "
            Task.err (CmdError (Str.concat "Unrecognized filepath tags: " namesStr))

subtractCommand = \names, gitEnv, filepathsByTag ->
    when tagsToFilepaths filepathsByTag names is
        Filepaths filepaths ->
            args = List.concat ["restore", "--staged"] filepaths
            Cmd.new gitEnv.gitBin
                |> Cmd.args args
                |> Cmd.status
                |> Task.mapErr \_ -> CmdError "Error executing git subtract command"
        Unrecognized unrecognizedNames ->
            namesStr = Str.joinWith unrecognizedNames ", "
            Task.err (CmdError (Str.concat "Unrecognized filepath tags: " namesStr))
