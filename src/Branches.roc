module []

import GitEnvironment exposing [GitEnv]

queryBranches : GitEnv, U64 ->
queryBranches = \gitEnv, count ->


callGitBranch = \git ->
    Cmd.new git
    |> Cmd.args ["branch", "--format='%(refname:short)'", "--sort=-committerdate"]
    |> Cmd.output
    |> Task.mapErr! \CmdOutputError err -> GitBranchFailed (Cmd.outputErrToStr err)
    |> .stdout
    |> Str.fromUtf8
    |> Result.withDefault ""
    |> Task.ok
