module [
    routeCommands
]

import Storage exposing [read]

routeCommands = \args,  ->
    rest = List.dropFirst args 1
    first = List.get args 0
    when args is
        ["add", ..] -> add rest
        _ -> Task.ok (List.concat "Unrecognized command: " first)

add = \names -> names
