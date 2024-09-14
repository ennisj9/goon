module [
    CommitLog,
    queryCommitLog,
    displayCommitLogs,
]

import Util exposing [truncateStr]
import pf.Cmd
import ansi.Core as Color

CommitLog : { hash : Str, shortDateTime : Str, author : Str, message : Str }

queryCommitLog : Str -> Task (List CommitLog) [GitLogFailed Str]
queryCommitLog = \git ->
    Cmd.new git
        |> Cmd.args ["log", "--pretty=%h|%ad|%ae|%s", "--date=format:%m/%d %H:%M", "-3"]
        |> Cmd.output
        |> Task.mapErr! \CmdOutputError err -> GitLogFailed (Cmd.outputErrToStr err)
        |> .stdout
        |> Str.fromUtf8
        |> Result.withDefault ""
        |> parseCommitLog
        |> Task.ok

displayCommitLogs : List CommitLog -> Str
displayCommitLogs = \logs ->
    List.mapWithIndex logs \log, i ->
        index =
            if i == 0 then
                Color.withFg "__" (Standard Green)
            else
                0 - (Num.toI8 i) |> Num.toStr
        hash =
            if i == 0 then
                Color.withFg log.hash (Standard Green)
            else
                log.hash
        author = Color.withFg log.author (Standard Yellow)
        authorAndMessage =
            Str.joinWith [author, ": ", log.message] ""
            |> truncateStr 65

        parts = [
            index,
            hash,
            log.shortDateTime,
            authorAndMessage,
        ]
        Str.joinWith parts " "
    |> Str.joinWith "\n"

parseCommitLog : Str -> List CommitLog
parseCommitLog = \logs ->
    Str.split logs "\n"
    |> List.keepIf \str -> str != ""
    |> List.map \line ->
        parts = Str.split line "|"

        email =
            List.get parts 2
            |> Result.withDefault ""
            |> Str.splitFirst "@"
            |> Result.map .before
            |> Result.withDefault ""
        emailName =
            Str.splitFirst email "+"
            |> Result.map .after
            |> Result.withDefault email

        {
            hash: Result.withDefault (List.get parts 0) "",
            shortDateTime: Result.withDefault (List.get parts 1) "",
            author: emailName,
            message: Result.withDefault (List.get parts 3) "",
        }

expect
    result = parseCommitLog "aae83c5|08/23 10:06|13029834+person@example.com|more git research\nblah"
    result
    == [
        {
            hash: "aae83c5",
            shortDateTime: "08/23 10:06",
            author: "person",
            message: "more git research",
        },
        {
            hash: "blah",
            shortDateTime: "",
            author: "",
            message: "",
        },
    ]
