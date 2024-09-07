module [
    nextTag,
    initialTag,
    Tag
]

import random.Random

alphabet = ["a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z", "2", "3", "4", "5", "6", "7", "8", "9"]

corpus = [
  "", "ben", "leo", "amy", "jon", "eve", "lee", "ann", "joe", "meg", "sam",
  "mia", "zoe", "ian", "roy", "max", "ray", "wes", "kim", "eva", "ash",
  "ace", "ali", "may", "sky", "cal", "uma", "val", "joy", "ivy", "gus",
  "lux", "eli", "dax", "kai", "ira", "ada", "rio", "asa"
]

State : Random.State U16
RandomGenerator : State-> { state : State, value : U16 }
Tag : { str: Str, index: U64, seed: State, generator: RandomGenerator}

initialTag : Tag
initialTag = {
    str: "",
    index: 0,
    seed: Random.seed16 43,
    generator: Random.u16 0 32
}

nextTag : Tag -> Tag
nextTag = \lastName ->
    if lastName.index < 38 then
        index = lastName.index + 1
        str = List.get corpus index |> Result.withDefault "ERR"
        { lastName & index, str }
    else
        { generator, seed } = lastName
        a = generator seed
        b = generator a.state
        c = generator b.state
        newSeed = c.state
        str =
            List.map [a, b, c] \random ->
                List.get alphabet (Num.toU64 random.value)
                |> Result.withDefault ""
            |> Str.joinWith ""
        {lastName & str, seed: newSeed}
