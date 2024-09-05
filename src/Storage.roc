module [
    writeStadusFile,
    readStadusFile,
    FileTags,
]

import json.Json
import pf.File
import pf.Task exposing [Task]

FileTags : List { filepath : Str, tag : Str }

writeStadusFile : FileTags -> Task {} [WriteFileErr Str]
writeStadusFile = \files ->
    content = Encode.toBytes files Json.utf8
    File.writeBytes ".git/stadus.json" content
        |> Task.mapErr! \_ -> WriteFileErr "Couldn't write .git/stadus.json"

readStadusFile : {} -> Task FileTags [ReadFileErr Str, DecodeFileErr Str]
readStadusFile = \{} ->
    decoded =
        File.readBytes ".git/stadus.json"
            |> Task.mapErr! \_ -> ReadFileErr "Coudn't read .git/stadus.json"
            |> Decode.fromBytes Json.utf8

    when decoded is
        Err _ -> Task.err (DecodeFileErr "Couldn't decode .git/stadus.json")
        Ok files -> Task.ok files
