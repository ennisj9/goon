module [
    writeFileTags,
    writeBranchTags,
    readContextOrFresh,
    TaggedValue,
    AppContext,
    usedTagsFromContext
]

import json.Json
import pf.File

TaggedValue : { tag : Str , value : Str }


AppContext : {
    files: List TaggedValue,
    branches: List TaggedValue
}

readContextOrFresh : Str -> Task AppContext []
readContextOrFresh = \dotGit ->
    Task.attempt (readContextFile dotGit) \result ->
        when result is
            Ok context -> Task.ok context
            Err _ -> Task.ok { files: [], branches: [] }


commitToFile = \dotGit, payload ->
    body = Encode.toBytes payload Json.utf8
    filename = Str.concat dotGit "/stadus.json"
    File.writeBytes filename body
        |> Task.mapErr! \_ -> WriteFileErr "Couldn't write .git/stadus.json"
    Task.ok {}

writeFileTags : Str, List TaggedValue -> Task {} [WriteFileErr Str]
writeFileTags = \dotGit, files ->
    existing = readContextOrFresh! dotGit
    content = { existing & files: files }
    commitToFile dotGit content

writeBranchTags : Str, List TaggedValue -> Task {} [WriteFileErr Str]
writeBranchTags = \dotGit, branches ->
    existing = readContextOrFresh! dotGit
    content = { existing & branches: branches }
    commitToFile dotGit content

tagSetFromList : List {tag: Str}* -> Set Str
tagSetFromList = \items ->
    List.walk items (Set.empty {}) \tags, item ->
        Set.insert tags item.tag

usedTagsFromContext : AppContext -> Set Str
usedTagsFromContext = \appContext ->
    Set.union (tagSetFromList appContext.files) (tagSetFromList appContext.branches)

expect
    context = {
        files: [{value: "Blah", tag: "a"},{value: "Foo", tag: "b"}],
        branches: [{value: "Bar", tag: "b"}, {value: "Foo", tag: "c"}]
    }
    result = usedTagsFromContext context
    result == Set.fromList ["a","b","c"]


readContextFile : Str -> Task AppContext [ReadFileErr Str, DecodeFileErr Str]
readContextFile = \dotGit ->
    decoded = File.readBytes (Str.concat dotGit "/stadus.json")
        |> Task.mapErr! \_ -> ReadFileErr "Coudn't read .git/stadus.json"
        |> Decode.fromBytes Json.utf8

    when decoded is
        Err _ -> Task.err (DecodeFileErr "Couldn't decode .git/stadus.json")
        Ok context -> Task.ok context
