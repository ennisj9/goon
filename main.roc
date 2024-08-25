app [main] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.14.0/dC5ceT962N_4jmoyoffVdphJ_4GlW3YMhAPyGPr-nU0.tar.br",
    parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.7.1/MvLlME9RxOBjl0QCxyn3LIaoG9pSlaNxCa-t3BfbPNc.tar.br",
    json: "https://github.com/lukewilliamboswell/roc-json/releases/download/0.10.1/jozYCvOqoYa-cV6OdTcxw3uDGn61cLvzr5dK1iKf1ag.tar.br",
}

import pf.Stdout
import pf.Cmd
import pf.Task exposing [Task]
import json.Json

gitExec = "/usr/bin/git"

main =
    queryCommitLog! gitExec
        |> Encode.toBytes Json.utf8
        |> Str.fromUtf8
        |> Result.withDefault "Couldn't read JSON"

# execExample = Cmd.exec "echo" ["EXEC"]

# stadus
# stadus discard <tag>  == git restore <file>
# stadus subtract <tag> == git restore --staged <file>
# stadus add <tag> == git add <file>
# stadus add . == git add .
# stadus commit "blah" == git commit -m "blah"
# stadus recommit == git commit --amend --no-edit
# stadus from <branch> <tag> == git checkout <branch> -- <file>
# status syncmain == git checkout main && git fetch main && git reset --hard

callGitStatus = \git ->
    Cmd.new git
        |> Cmd.args ["status", "--porcelain=2", "--branch", "--untracked-files=all"]
        |> Cmd.output
        |> Task.mapErr! \CmdOutputError err -> GitStatusFailed (Cmd.outputErrToStr err)
        |> .stdout
        |> Str.fromUtf8
        |> Result.withDefault "Couldn't parse git status as Utf8"
        |> Stdout.write
        |> Task.mapErr \_ -> GitStatusFailed "Error writing to stdout"

# X          Y     Meaning
# -------------------------------------------------
#          [AMD]   not updated
# M        [ MTD]  updated in index
# T        [ MTD]  type changed in index
# A        [ MTD]  added to index
# D                deleted from index
# R        [ MTD]  renamed in index
# C        [ MTD]  copied in index
# [MTARC]          index and work tree matches
# [ MTARC]    M    work tree changed since index
# [ MTARC]    T    type changed in work tree since index
# [ MTARC]    D    deleted in work tree
#             R    renamed in work tree
#             C    copied in work tree
# -------------------------------------------------
# M Changed        Change
# T Type changed   Type
# A Added          Add
# D Deleted        Delete
# R Renamed        Rename
# C Copied         Copy
# . Unchanged
#
# MTARC in index means it's "in', bout to be committed
# if work tree is "unchanged", it's the same as the index
# Green - indexState: MTARC + workTreeState: Unchanged

# Add / changed
# Delete / added

GitFile : [Tracked TrackedFile, Untracked UntrackedFile]
FileState : [Changed, TypeChanged, Added, Deleted, Renamed, Copied, Not]

TrackedFile : { filename : Str, indexState : FileState, workTreeState : FileState }
UntrackedFile : { filename : Str }

CommitLog : { hash : Str, shortDateTime : Str, author : Str, message : Str }

queryCommitLog : Str -> Task (List CommitLog) [GitStatusFailed Str]
queryCommitLog = \git ->
    Cmd.new git # |> Cmd.args ["status", "--porcelain=2", "--branch", "--untracked-files=all"]
        |> Cmd.args ["log", "--pretty=%h|%ad|%an|%s", "--date=format:%m/%d %H:%M", "-3"]
        |> Cmd.output
        |> Task.mapErr! \CmdOutputError err -> GitStatusFailed (Cmd.outputErrToStr err)
        |> .stdout
        |> Str.fromUtf8
        |> Result.withDefault ""
        |> parseCommitLog
        |> Task.ok

parseCommitLog : Str -> List CommitLog
parseCommitLog = \logs ->
    Str.split logs "\n"
    |> List.map \line ->
        parts = Str.split line "|"
        {
            hash: Result.withDefault (List.get parts 0) "",
            shortDateTime: Result.withDefault (List.get parts 1) "",
            author: Result.withDefault (List.get parts 2) "",
            message: Result.withDefault (List.get parts 3) "",
        }

expect
    parseCommitLog "aae83c5|08/23 10:06|Joe Ennis|more git research\nblah"
    == [
        {
            hash: "aae83c5",
            shortDateTime: "08/23 10:06",
            author: "Joe Ennis",
            message: "more git research",
        },
        {
            hash: "blah",
            shortDateTime: "",
            author: "",
            message: "",
        },
    ]

parseStatus : List u8 -> List GitFile

getPath : Str -> Result Str [NotFound]
getPath = \envVariables ->
    Str.split envVariables "\n"
        |> List.findFirst? \def -> Str.startsWith def "PATH"
        |> takeAfter "="

takeAfter : Str, Str -> Result Str [NotFound]
takeAfter = \str, seperator ->
    Str.splitFirst str seperator
    |> Result.map .after

expect getPath "ONE=blah\nPATH=some/path\nANOTHER=this" == Ok "some/path"
