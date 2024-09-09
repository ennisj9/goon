module [
    truncateStr,
    taskWithDefault
]

truncateStr = \str, maxLength -> # s
    if maxLength < 3 then
        "..."
    else
        asBytes = Str.toUtf8 str
        if List.len asBytes <= maxLength then
            str
        else
            asBytes
            |> List.sublist { start: 0, len: maxLength - 3 }
            |> \bytes ->
                Str.fromUtf8 bytes
                |> Result.withDefault str
                |> Str.concat "..."

expect truncateStr "some text" 9 == "some text"
expect truncateStr "some text" 7 == "some..."
expect truncateStr "some text" 3 == "..."
expect truncateStr "some text" 0 == "..."

taskWithDefault : Task a b, a -> Task a c
taskWithDefault = \task, default ->
    Task.attempt! task \result ->
        Task.ok (Result.withDefault result default)
