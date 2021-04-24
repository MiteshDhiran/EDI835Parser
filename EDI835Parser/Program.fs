// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open ParserLib.ParserModule
// Define a function to construct a message to print
let from whom =
    sprintf "from %s" whom

[<EntryPoint>]
let main argv =
    let message = from "F#" // Call the function
    printfn "Hello world %s" message
    let comma = pchar ','
    let oneOrMoreDigitList = sepBy1 digit comma 
    let digitListResult = run (sepBy1 pInt comma) "12,34"
    let anythingSeperatedByCommaResult = run (sepBy1 anyString comma ) "AA,BB"
    Console.ReadLine() |> ignore
    0 // return an integer exit code


    //let comma = pchar ','
    //let oneOrMoreDigitList = sepBy1 digit comma      
    ////example
    //let digitListResult = run (sepBy1 pInt comma) "12,34"