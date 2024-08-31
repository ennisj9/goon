import pf.Cmd
import pf.Task exposing [Task]


FileState : [Changed, TypeChanged, Added, Deleted, Renamed, Copied, Not]

GitStatus : { branch : Str, remoteBranch : Str, files : List FileStatus }
FileStatus : { filename : Str, indexState : FileState, workTreeState : FileState }

callGitStatus = \git ->
    Cmd.new git
        |> Cmd.args ["status", "--porcelain", "--branch", "--untracked-files=all"]
        |> Cmd.output
        |> Task.mapErr! \CmdOutputError err -> GitStatusFailed (Cmd.outputErrToStr err)
        |> .stdout
        |> Str.fromUtf8
        |> Result.withDefault "Couldn't parse git status as Utf8"
        |> Task.ok


queryGitStatus: Str -> Task ()
