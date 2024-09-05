module [
    queryGitStatus,
    FileWithStatus,
    FileStatus,
    fileStateToLabel,
]

import pf.Cmd
import pf.Task exposing [Task]

FileState : [Changed, Unchanged, TypeChanged, Added, Deleted, Renamed, Copied]

GitBranch : { name : Str, offset : I64 }

GitStatus : { localBranch : GitBranch, remoteBranch : [None, Some GitBranch], files : List FileWithStatus }
FileStatus : { indexState : FileState, workTreeState : FileState, movedFrom : [None, Some Str] }
FileWithStatus : { filepath : Str, status : FileStatus }

callGitStatus = \git ->
    Cmd.new git
        |> Cmd.args ["status", "--porcelain=2", "--branch", "--untracked-files=all"]
        |> Cmd.output
        |> Task.mapErr! \CmdOutputError err -> GitStatusFailed (Cmd.outputErrToStr err)
        |> .stdout
        |> Str.fromUtf8
        |> Result.withDefault ""
        |> Task.ok

queryGitStatus : Str -> Task GitStatus [GitStatusFailed Str]
queryGitStatus = \git ->
    statusOutput = callGitStatus! git
    lines = Str.split statusOutput "\n"
    result = List.walk lines { headers: [], files: [] } \state, line ->
        if Str.startsWith line "#" then
            { state & headers: List.append state.headers line }
        else
            { state & files: List.append state.files line }
    branchInfo = parseBranchHeaderLines result.headers
    files = parseFilesLines result.files
    Task.ok { localBranch: branchInfo.localBranch, remoteBranch: branchInfo.remoteBranch, files }

getStr : List Str, U64 -> Str
getStr = \list, index ->
    List.get list index |> Result.withDefault ""

getNumOr0 : List Str, U64 -> I64
getNumOr0 = \list, index ->
    List.get list index
    |> Result.withDefault "0"
    |> Str.toI64
    |> Result.withDefault 0

parseBranchHeaderLines : List Str -> { localBranch : GitBranch, remoteBranch : [None, Some GitBranch] }
parseBranchHeaderLines = \lines ->
    initial = { localName: "", localOffset: 0, remoteName: None, remoteOffset: 0 }
    extracted = List.walk lines initial \state, line ->
        parts = Str.split line " "
        when parts is
            ["#", "branch.head", ..] -> { state & localName: getStr parts 2 }
            ["#", "branch.upstream", ..] -> { state & remoteName: Some (getStr parts 2) }
            ["#", "branch.ab", ..] ->
                { state & localOffset: getNumOr0 parts 2, remoteOffset: getNumOr0 parts 3 }

            _ -> state
    remoteBranch =
        when extracted.remoteName is
            Some name -> Some { name, offset: extracted.remoteOffset }
            None -> None
    localBranch = { name: extracted.localName, offset: extracted.localOffset }
    { localBranch, remoteBranch }

expect
    example = "# branch.oid bc8687123469a6053ee8f6ee37eec3e532525edc\n# branch.head main\n# branch.upstream origin/main\n# branch.ab +1 -0"
    parsedExample = parseBranchHeaderLines (Str.split example "\n")
    parsedExample == { localBranch: { name: "main", offset: 1 }, remoteBranch: Some { name: "origin/main", offset: 0 } }

strToChars : Str -> List Str
strToChars = \str ->
    Str.toUtf8 str
    |> List.map \codeunit -> Str.fromUtf8 [codeunit] |> Result.withDefault ""

parseFilesLines : List Str -> List FileWithStatus
parseFilesLines = \lines ->
    # TODO: implement unmerged
    List.keepOks lines \line ->
        parts = Str.split line " "
        when parts is
            ["1", ..] -> Ok (parseRegularFile parts)
            ["2", ..] -> Ok (parseRenamedFile parts)
            ["?", ..] ->
                Ok { filepath: getStr parts 1, status: { indexState: Unchanged, workTreeState: Added, movedFrom: None } }

            _ -> Err "unrecognized"

# FileStatus : { filepath : Str, indexState : FileState, workTreeState : FileState, movedFrom : [None, Some Str] }

parseRegularFile : List Str -> FileWithStatus
parseRegularFile = \parts ->
    statusCode = getStr parts 1
    filepath = getStr parts 8
    status = parseStatusCode statusCode
    {
        filepath,
        status: {
            indexState: status.indexState,
            workTreeState: status.workTreeState,
            movedFrom: None,
        },
    }

parseRenamedFile : List Str -> FileWithStatus
parseRenamedFile = \parts ->
    statusCode = getStr parts 1
    filepaths = getStr parts 9 |> Str.split "\t"
    filepath = getStr filepaths 0
    movedFrom = Some (getStr filepaths 1)
    status = parseStatusCode statusCode
    {
        filepath,
        status: {
            indexState: status.indexState,
            workTreeState: status.workTreeState,
            movedFrom,
        },
    }

parseStatusCode : Str -> { indexState : FileState, workTreeState : FileState }
parseStatusCode = \code ->
    strChars = strToChars code
    indexCode = getStr strChars 0
    workTreeCode = getStr strChars 1
    indexState = statusCodeLetterToState indexCode
    workTreeState = statusCodeLetterToState workTreeCode
    { indexState, workTreeState }

# FileState : [Changed, Unchanged, TypeChanged, Added, Deleted, Renamed, Copied, Not]

statusCodeLetterToState : Str -> FileState
statusCodeLetterToState = \codeChar ->
    when codeChar is
        "." -> Unchanged
        "M" -> Changed
        "T" -> TypeChanged
        "A" -> Added
        "D" -> Deleted
        "R" -> Renamed
        "C" -> Copied
        _ -> Unchanged

fileStateToLabel : FileState -> Str
fileStateToLabel = \fileState ->
    when fileState is
        Unchanged -> "unchanged"
        Changed -> "changed"
        TypeChanged -> "typed  "
        Added -> "added  "
        Deleted -> "deleted"
        Renamed -> "renamed"
        Copied -> "copied "

# branch.head main
# branch.upstream origin/main
# branch.ab +1 -0
