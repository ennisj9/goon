module [
    runStatus
]

import ansi.Core as Color
import CommitLog exposing [queryCommitLog, displayCommitLogs]
import GitStatus exposing [queryGitStatus, FileWithStatus, fileStateToLabel, GitBranch, FileStatus]
import FileTag exposing [initialTag, firstUnusedTag]
import Storage exposing [writeFileTags, AppContext, usedTagsFromContext]

File : { filepath : Str, status : FileStatus, tag : Str }

runStatus = \{ dotGit, gitBin }, savedContext ->
    commitLogs = queryCommitLog! gitBin
    gitStatus = queryGitStatus! gitBin
    currentFiles = tagStatusFiles savedContext gitStatus.files
    newFileTags = List.map currentFiles \file -> { value: file.filepath, tag: file.tag }
    writeFileTags! dotGit newFileTags
    branchDisplay = displayBranches gitStatus
    logDisplay = displayCommitLogs commitLogs
    fileDisplay = displayFiles currentFiles
    Task.ok (Output (Str.joinWith [branchDisplay, logDisplay, fileDisplay] "\n"))

tagStatusFiles : AppContext, List FileWithStatus -> List File
tagStatusFiles = \savedContext, files ->
    usedTags = usedTagsFromContext savedContext
    tagsByFilepath =
        List.map savedContext.files \file -> (file.value, file.tag)
        |> Dict.fromList
    filesState = { currentFiles: [], currentTag: initialTag }
    List.walk files filesState \{ currentFiles, currentTag }, { filepath, status } ->
        { tagValue, tagState } =
            when Dict.get tagsByFilepath filepath is
                Ok existing -> { tagValue: existing, tagState: currentTag }
                Err KeyNotFound ->
                    newTag = firstUnusedTag currentTag usedTags
                    { tagValue: newTag.str, tagState: newTag }
        newFile = { filepath, tag: tagValue, status }
        { currentFiles: List.append currentFiles newFile, currentTag: tagState }
    |> .currentFiles

displayBranches : { localBranch : GitBranch, remoteBranch : [None, Some GitBranch] }* -> Str
displayBranches = \{ localBranch, remoteBranch } ->
    local = displayBranch localBranch
    when remoteBranch is
        None -> local
        Some branch ->
            remote = displayBranch branch
            Str.joinWith [local, "->", remote] " "

displayBranch : GitBranch -> Str
displayBranch = \{ name, offset } ->
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
        Color.withFg "no changes" (Standard Cyan)
    else
        join = \bucket -> Str.joinWith bucket "\n"
        join (List.dropIf [join staged, join mixed, join workTree] \x -> x == "")
