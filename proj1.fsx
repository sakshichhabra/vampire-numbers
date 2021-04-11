// open System.Diagnostics
#time "on"
#r @"bin/Debug/netcoreapp3.1/Akka.FSharp.dll"
#r @"bin/Debug/netcoreapp3.1/Akka.dll"


open System
open System.Diagnostics
open Akka.Actor
open Akka.FSharp

// command to add Akka package:->  dotnet add package Akka.FSharp --version 1.4.10


let system = ActorSystem.Create("FSharp")


let verifyPerfectSquare (n):bool =
    let sqroot = sqrt( float (n) )
    sqroot - floor(sqroot) = 0.0


let echoServer = 
    spawn system "EchoServer"
    <| fun mailbox ->
        let rec loop() =
            actor {
                let! message = mailbox.Receive()
                let sender = mailbox.Sender()
                match box message with
                | :? string as msg -> 
                    let words = msg.Split [|' '|]
                    let l = words.[0] |> int
                    let r = words.[1] |> int
                    let k = words.[2] |> int
                    // let checkSquares(l, r, k) =             
                        
                        
                    let mutable res: decimal = decimal 0
                    for start = l to l+k-1 do
                        res <- res + decimal (decimal(start)*decimal(start))

                    if verifyPerfectSquare(res) then
                        Console.WriteLine("{0}", l)

                    for start = l+1 to r do 
                        res <- res - decimal (decimal(start-1)*decimal(start-1))
                        res <- res + decimal (decimal(start + k - 1)*decimal(start + k - 1))

                        if verifyPerfectSquare(res) then
                            Console.WriteLine("{0}", start)
                    sender <! "response"
                    return! loop()
                | _ ->failwith "invalid input"
                } 
        loop()
        

let SquaresRange(n, k) = 
    let actorCount = 1000000


    let mutable batchSize = 0
    if (n/actorCount)*actorCount = n then 
        batchSize <- n/actorCount
    else
        batchSize <- n/actorCount + 1

    let mutable min = 0
    for i in 1 .. batchSize .. n do
        if i + batchSize - 1 < n then
            min <- i + batchSize - 1
        else
            min <- n
        
        // checkSquares(i, min, k)

        let l1  = string i
        let r1  = string min
        let k1  = string k

        let str = String.concat " " [| l1; r1; k1 |]

        try
            let task = (echoServer <? str)

            Async.RunSynchronously (task)|> ignore
            
        with :? TimeoutException ->
            printfn "ask: timeout!"
    system.Terminate|> ignore
    0

let args : string array = fsi.CommandLineArgs |> Array.tail 
let n =args.[0] |> int
let k =args.[1] |> int

let clock = Stopwatch()
clock.Start()

let currentProcess = Process.GetCurrentProcess()
let TimeStampCPU = currentProcess.TotalProcessorTime

SquaresRange(n,k) |>ignore

let realTime = clock.ElapsedMilliseconds
printfn "Real Time: %d ms" realTime
let cpuTime = int64 (currentProcess.TotalProcessorTime-TimeStampCPU).TotalMilliseconds
printfn "Cpu Time: %d ms"  cpuTime
if cpuTime > realTime then
        printfn "Concurrency Ratio: %f" ((float cpuTime)/ (float realTime))

exit(0)