module [
    writeStadusFile,
    readStadusFile,
    FileTags
]

import json.Json
import pf.File
import pf.Path exposing [Path]

FileTags : List { filepath : Str, tag : Str }

stadusJsonStr : Path -> Str
stadusJsonStr = \dotGit ->
    Str.concat (Path.display dotGit) "/stadus.json"

writeStadusFile : Path, FileTags ->  Task {} [WriteFileErr Str]
writeStadusFile = \dotGit, files ->
    content = Encode.toBytes files Json.utf8
    File.writeBytes (stadusJsonStr dotGit) content
        |> Task.mapErr! \err -> WriteFileErr (Inspect.toStr err) #"Couldn't write .git/stadus.json"

readStadusFile : Path -> Task FileTags [ReadFileErr Str, DecodeFileErr Str]
readStadusFile = \dotGit ->
    decoded =
        File.readBytes (stadusJsonStr dotGit)
            |> Task.mapErr! \_ -> ReadFileErr "Coudn't read .git/stadus.json"
            |> Decode.fromBytes Json.utf8

    when decoded is
        Err _ -> Task.err (DecodeFileErr "Couldn't decode .git/stadus.json")
        Ok files -> Task.ok files
