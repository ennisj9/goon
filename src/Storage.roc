module [
    writeStadusFile,
    readStadusFile,
    FileTags,
]

import json.Json
import pf.File
import pf.Path exposing [Path]

FileTags : List { filepath : Str, tag : Str }

writeStadusFile : FileTags -> Task {} [WriteFileErr Str]
writeStadusFile = \files ->
    content = Encode.toBytes files Json.utf8
    File.writeBytes ".git/stadus.json" content
        |> Task.mapErr! \_ -> WriteFileErr "Couldn't write .git/stadus.json"

readStadusFile : Path -> Task FileTags [ReadFileErr Str, DecodeFileErr Str]
readStadusFile = \dotGit ->
    filename = Str.concat (Path.display dotGit) "/stadus.json"
    decoded =
        File.readBytes filename
            |> Task.mapErr! \_ -> ReadFileErr "Coudn't read .git/stadus.json"
            |> Decode.fromBytes Json.utf8

    when decoded is
        Err _ -> Task.err (DecodeFileErr "Couldn't decode .git/stadus.json")
        Ok files -> Task.ok files
