app [main] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.15.0/SlwdbJ-3GR7uBWQo6zlmYWNYOxnvo8r6YABXD-45UOw.tar.br",
    parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.7.1/MvLlME9RxOBjl0QCxyn3LIaoG9pSlaNxCa-t3BfbPNc.tar.br",
    json: "https://github.com/lukewilliamboswell/roc-json/releases/download/0.10.1/jozYCvOqoYa-cV6OdTcxw3uDGn61cLvzr5dK1iKf1ag.tar.br",
    ansi: "https://github.com/lukewilliamboswell/roc-ansi/releases/download/0.5/1JOFFXrqOrdoINq6C4OJ8k3UK0TJhgITLbcOb-6WMwY.tar.br",
    random: "https://github.com/JanCVanB/roc-random/releases/download/v0.1.3/YfNiz9trKhsdZ7w_MdzfxW0U01Pw2iSzbrRfcIXhYPM.tar.br",
}

import pf.Stdout
import pf.Arg

import Storage exposing [readContextOrFresh]
import GitEnvironment exposing [getGitEnv]
import Commands exposing [routeCommands]
import "help.txt" as helpText : Str

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
    savedContext = readContextOrFresh! gitEnv.dotGit
    rawArgs = Arg.list! {}
    args = List.dropFirst rawArgs 1
    if List.isEmpty args then
        helpText
        |> Stdout.write
    else
        result = routeCommands! args gitEnv savedContext
        when result is
            Silent -> Task.ok {}
            Output output ->
                Stdout.write (Str.concat output "\n")

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
