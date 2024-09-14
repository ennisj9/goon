module [
    routeCommands
]

import pf.Cmd
import Storage exposing [FileAndTag]

routeCommands = \args, gitEnv, appContext  ->
    rest = List.dropFirst args 1
    first = List.get args 0
        |> Result.withDefault ""
    filepathsByTag = indexFileTags appContext.files
    when args is
        ["add", ..] -> addCommand rest gitEnv filepathsByTag
        ["subtract", ..] -> subtractCommand rest gitEnv filepathsByTag
        ["branches"] -> branchesCommand gitEnv
        _ -> Task.err (CmdError (Str.concat "Unrecognized command: " first))

indexFileTags : List FileAndTag -> Dict Str Str
indexFileTags = \savedFileTags ->
    List.walk savedFileTags (Dict.empty {}) \byTag, {filepath, tag} ->
        Dict.insert byTag tag filepath

expect
    files = [{filepath: "blah", tag: "a"}, {filepath: "foobar", tag: "b"}]
    index = indexFileTags files
    expected = Dict.fromList [("a","blah"),("b", "foobar")]
    index == expected

tagIndexFromList = \items, getter ->
    List.walk items (Dict.empty {}) \byTag, item ->
        value = getter item
        Dict.insert byTag item.tag value

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

branchesCommand = \gitEnv ->

    Task.ok (Output "blah")

addCommand = \names, gitEnv, filepathsByTag ->
    when tagsToFilepaths filepathsByTag names is
        Filepaths filepaths ->
            args = List.prepend filepaths "add"
            Cmd.new gitEnv.gitBin
                |> Cmd.args args
                |> Cmd.status
                |> Task.mapErr! \_ -> CmdError "Error executing git add command"
            Task.ok Silent
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
                |> Task.mapErr! \_ -> CmdError "Error executing git subtract command"
            Task.ok Silent
        Unrecognized unrecognizedNames ->
            namesStr = Str.joinWith unrecognizedNames ", "
            Task.err (CmdError (Str.concat "Unrecognized filepath tags: " namesStr))
