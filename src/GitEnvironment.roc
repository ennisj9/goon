module [
    getGitEnv,
]

import pf.Path exposing [Path]
import pf.Cmd
import pf.Dir

getGitEnv = \{} ->
    gitBin = findGitExecutablePath! {}
    dotGit = findGitFolderPath! {}
    Task.ok { gitBin, dotGit }

findGitFolderPath = \{} ->
    Task.loop! { pathStr: "./", depth: 0 } \{ pathStr, depth } ->
        if depth > 10 then
            Task.err (GitFolderNotFound "Looked in the 10 directories above the current working directory but found no .git")
        else
            result =
                Path.fromStr pathStr
                    |> Path.listDir!
                    |> findDotGitInFileList
            when result is
                Found path -> Task.ok (Done (Path.display path))
                NotFound ->
                    next = { pathStr: Str.concat pathStr "../", depth: depth + 1 }
                    Task.ok (Step next)

findDotGitInFileList : List Path -> [Found Path, NotFound]
findDotGitInFileList = \list ->
    List.walkUntil list NotFound \_, path ->
        file =
            Path.display path
            |> Str.splitLast "/"
            |> Result.withDefault { before: "", after: "" }
            |> .after
        if file == ".git" then Break (Found path) else Continue NotFound

findGitExecutablePath = \{} ->
    output =
        Cmd.new "env"
            |> Cmd.output
            |> Task.mapErr! \_ -> GitExecutableError "Could not access PATH with 'env'"
            |> .stdout
    pathDirs =
        output
        |> Str.fromUtf8
        |> Result.withDefault ""
        |> pathDirsFromEnvs
    when pathDirs is
        Ok dirs -> searchDirsForGitBinary! dirs
        Err _ -> Task.err (GitExecutableError "Could not find PATH in env variables")

searchDirsForGitBinary : List Str -> Task Str [GitExecutableError Str]
searchDirsForGitBinary = \dirs ->
    result = Task.loop! 0 \index ->
        when List.get dirs index is
            Err OutOfBounds -> Task.ok (Done (Err NotFound))
            Ok dir ->
                Task.attempt (Dir.list dir) \maybeFiles ->
                    when maybeFiles is
                        Ok files ->
                            maybeGit = List.findFirst files \file ->
                                filename =
                                    Str.splitLast (Path.display file) "/"
                                    |> Result.map .after
                                    |> Result.withDefault ""
                                filename == "git"
                            when maybeGit is
                                Ok gitPath -> Task.ok (Done (Ok gitPath))
                                Err _ -> Task.ok (Step (index + 1))

                        Err _ -> Task.ok (Step (index + 1))
    when result is
        Err _ -> Task.err (GitExecutableError "Could not find git executable in PATH")
        Ok path -> Task.ok (Path.display path)

pathDirsFromEnvs : Str -> Result (List Str) [NotFound]
pathDirsFromEnvs = \envs ->
    Str.split envs "\n"
        |> List.findFirst? \line -> Str.startsWith line "PATH"
        |> Str.splitFirst "="
        |> Result.map \{ after } ->
            Str.split after ":"

expect
    result = pathDirsFromEnvs "something\nFOO=BAR\nPATH=blah:more:here\n" |> Result.withDefault []
    result == ["blah", "more", "here"]
