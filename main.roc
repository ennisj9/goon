app [main] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.12.0/Lb8EgiejTUzbggO2HVVuPJFkwvvsfW6LojkLR20kTVE.tar.br",
    parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.7.1/MvLlME9RxOBjl0QCxyn3LIaoG9pSlaNxCa-t3BfbPNc.tar.br",
}

import pf.Stdout
import pf.Cmd
import pf.Task exposing [Task]

gitExec = "/usr/bin/git"

main =
    callGitStatus
        |> Task.mapErr! GitStatusError

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

callGitStatus =
    output =
        Cmd.new gitExec
            |> Cmd.args ["status", "--porcelain=2", "--branch", "--untracked-files=all"]
            |> Cmd.output
            |> Task.mapErr! \CmdOutputError err -> GitStatusFailed (Cmd.outputErrToStr err)
    output.stdout
    |> Str.fromUtf8
    |> Result.withDefault "Couldn't parse git status as Utf8"
    |> Stdout.write

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
# M Changed
# T Type changed
# A Added
# D Deleted
# R Renamed
# C Copied
#
# MTARC in index means it's "in', bout to be committed
# if work tree is "unchanged", it's the same as the index

GitFile : [Tracked TrackedFile, Untracked UntrackedFile]
FileState : [Changed, TypeChanged, Added, Deleted, Renamed, Copied, Unchanged]

TrackedFile : { filename : Str, indexState : FileState, workTreeState : FileState }
UntrackedFile : { filename : Str }

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

expect Result.withDefault (getPath "ONE=blah\nPATH=some/path\nANOTHER=this") "" == "some/path"
