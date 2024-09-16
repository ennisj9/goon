module [
    queryGitBranches,
    tagBranches,
]

import pf.Cmd
import Storage exposing [AppContext, usedTagsFromContext, TaggedValue]
import FileTag exposing [initialTag, firstUnusedTag]

queryGitBranches : Str -> Task (List Str) [GitBranchFailed Str]
queryGitBranches = \gitBin ->
    callGitBranch! gitBin
        |> Str.split "\n"
        |> List.dropIf \str -> str == ""
        |> Task.ok

callGitBranch : Str -> Task Str [GitBranchFailed Str]
callGitBranch = \git ->
    Cmd.new git
        |> Cmd.args ["branch", "--format=%(refname:short)", "--sort=-committerdate"]
        |> Cmd.output
        |> Task.mapErr! \CmdOutputError err -> GitBranchFailed (Cmd.outputErrToStr err)
        |> .stdout
        |> Str.fromUtf8
        |> Result.withDefault ""
        |> Task.ok

tagBranches : AppContext, List Str -> List TaggedValue
tagBranches = \savedContext, branches ->
    usedTags = usedTagsFromContext savedContext
    tagsByBranch =
        List.map savedContext.branches \branchPair -> (branchPair.value, branchPair.tag)
        |> Dict.fromList
    initialState = { currentBranches: [], currentTag: initialTag }
    List.walk branches initialState \{ currentBranches, currentTag }, branch ->
        { tagValue, tagState } =
            when Dict.get tagsByBranch branch is
                Ok existing -> { tagValue: existing, tagState: currentTag }
                Err KeyNotFound ->
                    newTag = firstUnusedTag currentTag usedTags
                    { tagValue: newTag.str, tagState: newTag }
        newBranch = { value: branch, tag: tagValue }
        { currentBranches: List.append currentBranches newBranch, currentTag: tagState }
    |> .currentBranches
