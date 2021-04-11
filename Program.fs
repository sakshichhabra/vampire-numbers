// open System.Diagnostics

// #r @"bin/Debug/netcoreapp3.1/Akka.TestKit.dll"

open System


// command to add Akka package:->  dotnet add package Akka.FSharp --version 1.4.10
open Akka.Actor
open Akka.Configuration
open Akka.FSharp


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
    let actorCount = 10

    
    // let echoServers = 
    //     [1 .. actorCount]
    //     |> List.map(fun id ->   let properties = [| string(id) :> obj |]
    //                             system.ActorOf(Props(typedefof<EchoServer>, properties)))

    // let rand = Random(1234)

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
    system.Terminate
        


[<EntryPoint>]
let main argv =

    let n:int = int argv.[0]
    let k:int = int argv.[1]
    SquaresRange(n,k) |>ignore
    // let l1  = string 1
    // let r1  = string n
    // let k1  = string k

    // let str = String.concat " " [| l1; r1; k1 |]

    // for timeout in [10; 100; 250; 2500] do
    //     try
    //         let task = (echoServer <? (str))

    //         Async.RunSynchronously (task, timeout)
            
    //     with :? TimeoutException ->
    //         printfn "ask: timeout!"
    //     system.Stop(echoServer)
    0