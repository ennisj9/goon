module [
    routeCommands
]

import pf.Cmd
import ansi.Core as Color
import Storage exposing [TaggedValue, writeBranchTags]
import Branches exposing [queryGitBranches, tagBranches]

routeCommands = \args, gitEnv, appContext  ->
    rest = List.dropFirst args 1
    first = List.get args 0
        |> Result.withDefault ""

    branchesByTag = indexTags appContext.branches
    filepathsByTag = indexTags appContext.files
    when args is
        ["add", "."] -> addAllCommand gitEnv
        ["recommit"] -> recommitCommand gitEnv
        ["add", ..] -> addCommand gitEnv filepathsByTag rest
        ["subtract", ..] -> subtractCommand gitEnv filepathsByTag rest
        ["restore", ..] -> restoreCommand gitEnv filepathsByTag rest
        ["branches"] -> branchesCommand gitEnv appContext Default
        ["branches", count] -> branchesCommand gitEnv appContext (Specific count)
        ["switch", tag] -> switchCommand gitEnv branchesByTag tag
        _ -> Task.err (CmdError (Str.concat "Unrecognized command: " first))

indexTags : List TaggedValue -> Dict Str Str
indexTags = \savedFileTags ->
    List.walk savedFileTags (Dict.empty {}) \byTag, {value, tag} ->
        Dict.insert byTag tag value
        |> Dict.insert value value

expect
    files = [{value: "blah", tag: "a"}, {value: "foobar", tag: "b"}]
    index = indexTags files
    expected = Dict.fromList [("a","blah"),("b", "foobar")]
    index == expected

tagsToValues : Dict Str Str, List Str -> [Values (List Str), Unrecognized (List Str)]
tagsToValues = \tagToValueMap, tags ->
    inspected = List.walk tags { unrecognized: [], values: [] } \state, tag ->
        when Dict.get tagToValueMap tag is
            Ok value -> { state & values: List.append state.values value }
            Err KeyNotFound -> { state & unrecognized: List.append state.unrecognized tag }
    if List.isEmpty inspected.unrecognized then
        Values inspected.values
    else
        Unrecognized inspected.unrecognized

branchesCommand = \gitEnv, appContext, countArg ->
    branches = queryGitBranches! gitEnv.gitBin
    maxLength = when countArg is
        Specific countStr -> Str.toU64 countStr |> Result.withDefault 5
        Default -> 5
    someBranches = List.takeFirst branches maxLength
    taggedBranches = tagBranches appContext someBranches
    writeBranchTags! gitEnv.dotGit taggedBranches
    List.map taggedBranches \{tag, value} ->
        Str.joinWith [Color.withFg tag (Standard Yellow), " ", value] ""
    |> Str.joinWith "\n"
    |> Output
    |> Task.ok

switchCommand = \gitEnv, branchesByTag, tag ->
    when Dict.get branchesByTag tag is
        Ok branch ->
            args = ["switch", branch]
            Cmd.new gitEnv.gitBin
                |> Cmd.args args
                |> Cmd.status
                |> Task.mapErr! \_ -> CmdError "Error executing git switch command"
            Task.ok Silent
        Err KeyNotFound ->
            Task.err (CmdError (Str.concat "Unrecognized branch tag: " tag))

addCommand = \gitEnv, filepathsByTag, tags ->
    when tagsToValues filepathsByTag tags is
        Values filepaths ->
            args = List.prepend filepaths "add"
            Cmd.new gitEnv.gitBin
                |> Cmd.args args
                |> Cmd.status
                |> Task.mapErr! \_ -> CmdError "Error executing git add command"
            Task.ok Silent
        Unrecognized unrecognizedNames ->
            tagsStr = Str.joinWith unrecognizedNames ", "
            Task.err (CmdError (Str.concat "Unrecognized filepath tags: " tagsStr))

addAllCommand = \gitEnv ->
    Cmd.new gitEnv.gitBin
        |> Cmd.args ["add", "."]
        |> Cmd.status
        |> Task.mapErr! \_ -> CmdError "Error executing git add ."
    Task.ok Silent

recommitCommand = \gitEnv ->
    Cmd.new gitEnv.gitBin
        |> Cmd.args ["commit", "--amend", "--no-edit"]
        |> Cmd.status
        |> Task.mapErr! \_ -> CmdError "Error executing git commit --amend --no-edit"
    Task.ok Silent

subtractCommand = \gitEnv, filepathsByTag, tags ->
    when tagsToValues filepathsByTag tags is
        Values filepaths ->
            args = List.concat ["restore", "--staged"] filepaths
            Cmd.new gitEnv.gitBin
                |> Cmd.args args
                |> Cmd.status
                |> Task.mapErr! \_ -> CmdError "Error executing git restore --staged command"
            Task.ok Silent
        Unrecognized unrecognizedNames ->
            tagsStr = Str.joinWith unrecognizedNames ", "
            Task.err (CmdError (Str.concat "Unrecognized filepath tags: " tagsStr))

restoreCommand = \gitEnv, filepathsByTag, tags ->
    when tagsToValues filepathsByTag tags is
        Values filepaths ->
            args = List.prepend filepaths "restore"
            Cmd.new gitEnv.gitBin
                |> Cmd.args args
                |> Cmd.status
                |> Task.mapErr! \_ -> CmdError "Error executing git restore command"
            Task.ok Silent
        Unrecognized unrecognizedNames ->
            tagsStr = Str.joinWith unrecognizedNames ", "
            Task.err (CmdError (Str.concat "Unrecognized filepath tags: " tagsStr))
