module [
    writeStadusFile,
    readStadusFile,
    FileTags,
    FileAndTag
]

import json.Json
import pf.File

FileAndTag : { filepath : Str, tag : Str }
FileTags : List FileAndTag

writeStadusFile : Str, List FileAndTag -> Task {} [WriteFileErr Str]
writeStadusFile = \dotGit, files ->
    content = Encode.toBytes files Json.utf8
    filename = Str.concat dotGit "/stadus.json"
    File.writeBytes filename content
        |> Task.mapErr! \_ -> WriteFileErr "Couldn't write .git/stadus.json"

readStadusFile : Str -> Task FileTags [ReadFileErr Str, DecodeFileErr Str]
readStadusFile = \dotGit ->
    decoded = File.readBytes (Str.concat dotGit "/stadus.json")
        |> Task.mapErr! \_ -> ReadFileErr "Coudn't read .git/stadus.json"
        |> Decode.fromBytes Json.utf8

    when decoded is
        Err _ -> Task.err (DecodeFileErr "Couldn't decode .git/stadus.json")
        Ok files -> Task.ok files
