app [main] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.14.0/dC5ceT962N_4jmoyoffVdphJ_4GlW3YMhAPyGPr-nU0.tar.br",
    parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.7.1/MvLlME9RxOBjl0QCxyn3LIaoG9pSlaNxCa-t3BfbPNc.tar.br",
    json: "https://github.com/lukewilliamboswell/roc-json/releases/download/0.10.1/jozYCvOqoYa-cV6OdTcxw3uDGn61cLvzr5dK1iKf1ag.tar.br",
    ansi: "https://github.com/lukewilliamboswell/roc-ansi/releases/download/0.5/1JOFFXrqOrdoINq6C4OJ8k3UK0TJhgITLbcOb-6WMwY.tar.br",
}

import pf.Stdout
import pf.Cmd
import pf.Task exposing [Task]
import CommitLog exposing [queryCommitLog, displayCommitLogs]
import ansi.Core

gitExec = "/usr/bin/git"

main =
    outpuLogsAndStatus! gitExec
        |> Stdout.write
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

outpuLogsAndStatus = \git ->
    commitLogs = queryCommitLog! git |> displayCommitLogs |> Core.withFg (Standard Cyan)
    status = callGitStatus! git
    [commitLogs, status]
    |> Str.joinWith "\n"
    |> Task.ok

truncateStr = \str, maxLength -> # s
    if maxLength < 3 then
        "..."
    else
        asBytes = Str.toUtf8 str
        if List.len asBytes <= maxLength then
            str
        else
            asBytes
            |> List.sublist { start: 0, len: maxLength - 3 }
            |> \bytes ->
                Str.fromUtf8 bytes
                |> Result.withDefault str
                |> Str.concat "..."

expect truncateStr "some text" 9 == "some text"
expect truncateStr "some text" 7 == "some..."
expect truncateStr "some text" 3 == "..."
expect truncateStr "some text" 0 == "..."

callGitStatus = \git ->
    Cmd.new git
        |> Cmd.args ["status", "--porcelain", "--branch", "--untracked-files=all"]
        |> Cmd.output
        |> Task.mapErr! \CmdOutputError err -> GitStatusFailed (Cmd.outputErrToStr err)
        |> .stdout
        |> Str.fromUtf8
        |> Result.withDefault "Couldn't parse git status as Utf8"
        |> Task.ok

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

FileState : [Changed, TypeChanged, Added, Deleted, Renamed, Copied, Not]

GitStatus : { branch : Str, remoteBranch : Str, files : List FileStatus }
FileStatus : { filename : Str, indexState : FileState, workTreeState : FileState }

# parseStatus : Str -> GitStatus
# parseStatus = \str ->
#    { before, after } = Str.splitFirst str "\n"

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
