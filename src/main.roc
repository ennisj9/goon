app [main] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.15.0/SlwdbJ-3GR7uBWQo6zlmYWNYOxnvo8r6YABXD-45UOw.tar.br",
    parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.7.1/MvLlME9RxOBjl0QCxyn3LIaoG9pSlaNxCa-t3BfbPNc.tar.br",
    json: "https://github.com/lukewilliamboswell/roc-json/releases/download/0.10.1/jozYCvOqoYa-cV6OdTcxw3uDGn61cLvzr5dK1iKf1ag.tar.br",
    ansi: "https://github.com/lukewilliamboswell/roc-ansi/releases/download/0.5/1JOFFXrqOrdoINq6C4OJ8k3UK0TJhgITLbcOb-6WMwY.tar.br",
    random: "https://github.com/JanCVanB/roc-random/releases/download/v0.1.3/YfNiz9trKhsdZ7w_MdzfxW0U01Pw2iSzbrRfcIXhYPM.tar.br",
}

import pf.Stdout
import pf.Path
import pf.Arg
import ansi.Core as Color
import CommitLog exposing [queryCommitLog, displayCommitLogs]
import GitStatus exposing [queryGitStatus, FileWithStatus, FileStatus, fileStateToLabel, GitBranch]
import FileTag exposing [nextTag, initialTag, Tag]
import Storage exposing [writeStadusFile, readStadusFile, FileAndTag]
import GitEnvironment exposing [getGitEnv]
import Util exposing [taskWithDefault]
import Commands exposing [routeCommands]

File : { filepath : Str, status : FileStatus, tag : Str }

# stadus
# stadus discard <tag>  == git restore <file>
# stadus subtract <tag> == git restore --staged <file>
# stadus add <tag> == git add <file>
# stadus add . == git add .
# stadus commit "blah" == git commit -m "blah"
# stadus recommit == git commit --amend --no-edit
# stadus from <branch> <tag> == git checkout <branch> -- <file>
# status syncmain == git checkout main && git fetch main && git reset --hard


main =
    gitEnv = getGitEnv! {}
    savedFileTags = taskWithDefault! (readStadusFile gitEnv.dotGit) []
    rawArgs = Arg.list! {}
    args = List.dropFirst rawArgs 1
    if List.isEmpty args then
        runStatus! gitEnv savedFileTags
            |> Stdout.write
    else
        routeCommands args gitEnv savedFileTags


runStatus = \{dotGit , gitBin}, savedFileTags ->
    commitLogs = queryCommitLog! gitBin
    gitStatus = queryGitStatus! gitBin
    currentFiles = tagStatusFiles savedFileTags gitStatus.files
    newFileTags = List.map currentFiles \file -> { filepath: file.filepath, tag: file.tag }
    writeStadusFile! dotGit newFileTags
    branchDisplay = displayBranches gitStatus
    logDisplay = displayCommitLogs commitLogs
    fileDisplay = displayFiles currentFiles
    Task.ok (Str.concat (Str.joinWith [branchDisplay, logDisplay, fileDisplay] "\n") "\n")



tagStatusFiles : List FileAndTag, List FileWithStatus -> List File
tagStatusFiles = \savedFileTags, files ->
    savedInitial = { byFilepath: Dict.empty {}, usedTags: Set.empty {} }
    context = List.walk savedFileTags savedInitial \{ byFilepath, usedTags }, { filepath, tag } -> {
        byFilepath: Dict.insert byFilepath filepath tag,
        usedTags: Set.insert usedTags tag,
    }
    filesState = { currentFiles: [], currentTag: initialTag }
    List.walk files filesState \{ currentFiles, currentTag }, { filepath, status } ->
        { tagValue, tagState, isNew } =
            when Dict.get context.byFilepath filepath is
                Ok existing -> { tagValue: existing, tagState: currentTag, isNew: New }
                Err KeyNotFound ->
                    newTag = firstUnusedTag currentTag context.usedTags
                    { tagValue: newTag.str, tagState: newTag, isNew: Old }
        newFile = { filepath, tag: tagValue, status }
        { currentFiles: List.append currentFiles newFile, currentTag: tagState }
    |> .currentFiles

displayBranches : { localBranch: GitBranch, remoteBranch: [None, Some GitBranch] }* -> Str
displayBranches = \{ localBranch, remoteBranch } ->
    local = displayBranch localBranch
    when remoteBranch is
        None -> local
        Some branch ->
            remote = displayBranch branch
            Str.joinWith [local, "->", remote] " "

displayBranch : GitBranch -> Str
displayBranch = \{name, offset} ->
    if offset |> Num.isGt 0 then
        offsetStr = Color.withFg (Str.concat " +" (Num.toStr offset)) (Standard Yellow)
        Str.concat name offsetStr
    else if offset |> Num.isLt 0 then
        offsetStr = Color.withFg (Str.concat " -" (Num.toStr offset)) (Standard Red)
        Str.concat name offsetStr
    else
        name

displayFiles : List File -> Str
displayFiles = \files ->
    buckets = { staged: [], mixed: [], workTree: [] }
    printTag = \tag -> Color.withFg tag (Standard Cyan)
    { staged, mixed, workTree } = List.walk files buckets \state, { filepath, status, tag } ->
        when status.indexState is
            Unchanged ->
                workTreeLabel = Color.withFg (fileStateToLabel status.workTreeState) (Standard Red)
                str = Str.joinWith [printTag tag, workTreeLabel, filepath] " "
                { state & workTree: List.append state.workTree str }

            _ ->
                indexLabel = Color.withFg (fileStateToLabel status.indexState) (Standard Green)
                when status.workTreeState is
                    Unchanged ->
                        str = Str.joinWith [printTag tag, indexLabel, filepath] " "
                        { state & staged: List.append state.staged str }

                    _ ->
                        workTreeLabel = Color.withFg (fileStateToLabel status.workTreeState) (Standard Yellow)
                        first = Str.joinWith [printTag tag, indexLabel, filepath] " "
                        second = Str.joinWith ["   ", workTreeLabel, filepath] " "
                        { state & mixed: List.concat state.mixed [first, second] }
    if List.isEmpty files then
        Color.withFg  "no changes" (Standard Cyan)
    else
        join = \bucket -> Str.joinWith bucket "\n"
        join (List.dropIf [join staged, join mixed, join workTree] \x -> x == "")

firstUnusedTag = \currentTag, usedTags ->
    newTag = nextTag currentTag
    if !(Set.contains usedTags newTag.str) then
        newTag
    else
        firstUnusedTag newTag usedTags



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
