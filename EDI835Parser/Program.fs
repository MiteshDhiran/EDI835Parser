// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open ParserLib.ParserModule
open EDI.EDIModule

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
    let ediString = System.IO.File.ReadAllText(@"C:\Users\MDHIRAN\source\repos\EDIParser\EDI835Parser\Sample835.txt",System.Text.Encoding.ASCII)
    //let ediString = System.IO.File.ReadAllText(@"C:\Users\MDHIRAN\source\repos\EDIParser\EDI835Parser\Sample835.txt")
    let segmentSeperatorString = "GS" 
    //@"\r\n"
    //let stringParserExceptString = pStringBeforeString "GS" <|> anyString
    //let stringParserExceptString = getSepByStringParser "GS"
    //let segmentSeperatorParser = pString "GS" //pString segmentSeperatorString
    //let hardCodedEDIString = @"ABC\r\nXYZ\r\nNXC"
    //let segmentsA = run newPs hardCodedEDIString
    //let finalList = getSegmentsWithLoopIdentifierFromEDIContent ediString
    let tree = getTree ediString
    printf "%s" tree  
    //iterate over tree so that sequence gets executed
    (*
    let totalSize fileSystemItem =
        let fFile acc (file:SegmentInfo) = 
            acc + file.fields.Length
        let fDir acc (dir:PureDirectory)= 
            acc 
        EDI.EDIModule.fold fFile fDir 0 fileSystemItem

    let size = totalSize tree
    *)

    (*let segmentResult = run (getSepByStringParser Environment.NewLine) ediString
    let segments =  match segmentResult with
                           | Success (l,_) -> l |> List.toArray
                           |  Failure f -> [||]
    let getSegmentToFields segmentString =
        let segmentToFieldList = run (getSepByStringParser "*") segmentString
        match segmentToFieldList with
                | Success (l,_) -> l |> List.toArray
                | Failure f -> [||]
    let segmentWithFields = Array.map (fun segLine -> getSegmentToFields segLine) segments
    
    let zz = getRawSegmentRecordData segmentWithFields*)

    Console.ReadLine() |> ignore
    0 // return an integer exit code
    

    //ISA Level 1 END ISE
    //GS Level 2 END GE
    //ST LEVEL 3 END SE
    //LX LEVEL 4 END LQ
    //CLP LEVEL 5 END IS CLP OR LQ

    //LEVEL # SEQ # 
    //Loop LOOP or SEG []

    
        
  