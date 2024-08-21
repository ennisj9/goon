app [main] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.12.0/Lb8EgiejTUzbggO2HVVuPJFkwvvsfW6LojkLR20kTVE.tar.br",
}

import pf.Stdout
import pf.Cmd
import pf.Task exposing [Task]

main =
    outputExample |> Task.mapErr! OutputErr

# execExample = Cmd.exec "echo" ["EXEC"]

# Run "env" with verbose option, clear all environment variables, and pass in
# only as an environment variable "FOO"
outputExample : Task {} _
outputExample =
    output =
        Cmd.new "env"
            |> Cmd.output
            |> Task.mapErr! \CmdOutputError err -> EnvFailed (Cmd.outputErrToStr err)

    output.stdout
    |> Str.fromUtf8
    |> Result.try getPath
    |> Result.withDefault "Failed to decode PATH"
    |> Stdout.write

takeAfter : Str -> Result Str [NotFound]
takeAfter = \s ->
    Str.splitFirst s "="
    |> Result.map \parts -> parts.after

getPath : Str -> Result Str [NotFound]
getPath = \envVariables ->
    Str.split envVariables ("\n")
    |> List.findFirst \def -> Str.startsWith def "PATH"
    |> Result.try takeAfter

expect Result.withDefault (getPath "ONE=blah\nPATH=some/path\nANOTHER=this") "" == "some/path"
