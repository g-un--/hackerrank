open System
open System.Text
open System.Collections.Generic

type Op = 
    | IncrDp
    | DecrDp
    | IncrDpVal
    | DecrDpVal
    | Print
    | Read
    | While of Op list

let readLines count = 
    let mutable builder = StringBuilder()
    builder <- seq { 1..count }
               |> Seq.map (fun _ -> Console.ReadLine().Trim())
               |> Seq.fold (fun builder line -> builder.Append(line)) builder
    builder.ToString()

let readChars count = 
    Console.ReadLine().Trim() |> Seq.take count |> Array.ofSeq |> String 

let getReadFunc (input : string) =
    let mutable index = 0
    let read () = 
        let current = index
        index <- current + 1
        input.[current]
    read

let enqueue (opQueue : Queue<Op>) value = 
    opQueue.Enqueue(value)
    opQueue

let rec parse (program : char[]) (index : int) (opQueue : Queue<Op>) = 
    if index = program.Length then (opQueue |> List.ofSeq, index)
    else
        let next = index + 1
        match program.[index] with
        | '>' -> parse program next (enqueue opQueue IncrDp)
        | '<' -> parse program next (enqueue opQueue DecrDp)
        | '+' -> parse program next (enqueue opQueue IncrDpVal)
        | '-' -> parse program next (enqueue opQueue DecrDpVal)
        | '.' -> parse program next (enqueue opQueue Print)
        | ',' -> parse program next (enqueue opQueue Read)
        | '[' -> 
            let (innerTree, innerIndex) = parse program next (Queue<Op>()) 
            let whileNode = While(innerTree)
            parse program (innerIndex + 1) (enqueue opQueue whileNode)
        | ']' -> (opQueue |> List.ofSeq, index)
        | _ -> parse program next opQueue

let plus1 (reference : int byref) = 
    reference <- reference + 1

let minus1 (reference : int byref) = 
    reference <- reference - 1

let limit count = count >= 100_000

let rec interpret (memory : byte array) (dp : int byref) (instrCount : int byref) (program : Op list) (read : unit -> char) = 
    match program with
    | [] -> false
    | head :: tail ->
        if limit instrCount then
            true
        else
            match head with
            | IncrDp -> 
                plus1 &dp
                plus1 &instrCount
                interpret memory &dp &instrCount tail read 
            | DecrDp -> 
                minus1 &dp
                plus1 &instrCount
                interpret memory &dp &instrCount tail read
            | IncrDpVal -> 
                memory.[dp] <- memory.[dp] + 1uy
                plus1 &instrCount
                interpret memory &dp &instrCount tail read
            | DecrDpVal ->
                memory.[dp] <- memory.[dp] - 1uy
                plus1 &instrCount
                interpret memory &dp &instrCount tail read
            | Print ->
                Console.Write((char)memory.[dp])
                plus1 &instrCount
                interpret memory &dp &instrCount tail read
            | Read ->
                let value = read() |> byte
                memory.[dp] <- value
                plus1 &instrCount
                interpret memory &dp &instrCount tail read
            | While ops ->
                plus1 &instrCount
                if memory.[dp] = 0uy then 
                    plus1 &instrCount
                    interpret memory &dp &instrCount tail read
                else
                    let limitReached = interpret memory &dp &instrCount ops read
                    if limitReached then limitReached
                    else
                        plus1 &instrCount
                        if memory.[dp] = 0uy then interpret memory &dp &instrCount tail read 
                        else interpret memory &dp &instrCount program read

[<EntryPoint>]
let main _ = 
    match Console.ReadLine().Split ' ' |> Array.map int with
    | [| n; m |] -> 
        let input = readChars n
        let program = (readLines m).ToCharArray()
        let memory = Array.zeroCreate<byte> 5000
        let read = getReadFunc input
        let tree = parse program 0 (Queue<Op>())
        let mutable count = 0
        let mutable dp = 0
        let limitReached = interpret memory &dp &count (fst tree) read
        if limitReached then 
            Console.Write(Environment.NewLine + "PROCESS TIME OUT. KILLED!!!");
    | _ -> ()
    0