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
    let p = pStringBeforeString "," <|> anyString
    let working = pStringBeforeString ","
    let stringListResult = run (sepBy1 p comma) "AA,BB"
    let ediString = System.IO.File.ReadAllText(@"C:\Users\MDHIRAN\source\repos\EDIParser\EDI835Parser\Sample835.txt")
    let segmentSeperatorString = "GS" 
    //@"\r\n"
    //let stringParserExceptString = pStringBeforeString "GS" <|> anyString
    //let stringParserExceptString = getSepByStringParser "GS"
    //let segmentSeperatorParser = pString "GS" //pString segmentSeperatorString
    let newPs = getSepByStringParser "GS"
    let segments = run newPs "HelloGSWorldGSGood"
    //let segments = run (sepBy stringParserExceptString segmentSeperatorParser ) "HelloGSWorldGSGood"
    Console.ReadLine() |> ignore
    0 // return an integer exit code
    

    