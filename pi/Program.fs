open System

let builtinRandom = System.Random()
let cryptoprovider = System.Security.Cryptography.RNGCryptoServiceProvider.Create()

let cryptoRandom max =
    let bytes: byte [] = [|0uy; 0uy; 0uy; 0uy|]
    cryptoprovider.GetBytes bytes
    bytes.[3] <- bytes.[3] >>> 1
    BitConverter.ToInt32(bytes, 0) % max

let mutable state0: uint64 = uint64(1);
let mutable state1: uint64 = uint64(2);
let xorshiftRandom max =
    let mutable s1 = state0
    let mutable s0 = state1
    state0 <- s0
    s1 <- s1 ^^^ (s1 <<< 23)
    s1 <- s1 ^^^ (s1 >>> 17)
    s1 <- s1 ^^^ s0
    s1 <- s1 ^^^ (s0 >>> 26)
    state1 <- s1
    (state0 + state1) % uint64(max) |> int

let throw rng x =
    (rng x, rng x)

let offsetFromCenter size x =
    int64(x - size / 2 |> abs)

let distance size coords =
    let dist = offsetFromCenter size
    match coords with
    | (x, y) -> ((dist x |> pown) 2) + ((dist y |> pown) 2) |> double |> sqrt

let inside size coords =
    distance size coords <= double (size / 2)

let rec pirec iterations size rng incount outcount =
    let tail = pirec iterations size rng
    let inside = inside size

    if incount + outcount > iterations then
        double 4 / (double (incount + outcount) / double incount)
    else
        match throw rng size |> inside with
        | true -> tail (incount + 1) outcount
        | false -> tail incount (outcount + 1)

let pi iterations size rng =
    pirec iterations size rng 0 0

[<EntryPoint>]
let main _ =
    //config       iterations  box size
    let picfg = pi 50_000_000  100_000_000

    printfn "%f" <| picfg xorshiftRandom
    printfn ""
    printfn "%f" <| picfg builtinRandom.Next
    printfn ""
    printfn "%f" <| picfg cryptoRandom
    0
