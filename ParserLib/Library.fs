namespace ParserLib

module ParserModule =
    
    open System
    
    type Result<'a> =
        | Success of 'a
        | Failure of string

    type ParserLabel = string

    type ParserWithLabel<'a> = 
        {
            ParseFn : (string -> Result<'a * string>)
            Label:  ParserLabel 
        }

    type Parser<'a> = Parser of (string -> Result<'a * string>)

    
    let satisfy predicate =
        let innerFn input =
            if String.IsNullOrEmpty(input) then
                Failure "No More Input"
            else 
                let first = input.[0]
                if predicate first then
                    Success (first,input.[1..])
                else
                    let err = sprintf($"Unexpected {first}")
                    Failure err
        Parser innerFn                

    let pchar charToMatch =
        let innerFn input = 
            if String.IsNullOrEmpty(input) then
                Failure "No More Input"
            else
               let first = input.[0]
               if first = charToMatch then
                  let remaining = input.[1..]
                  Success(first,remaining)
               else
                   let msg = sprintf($"Expecting {charToMatch}. Got {first}")
                   Failure msg
        Parser innerFn           

    let run parser input =
        //unwrap the inner function
        let (Parser innerFunc) = parser
        //call inner function
        innerFunc input    

    let andThen firstParser secondParser =  
        let innerFn input =  
             let firstParserResult = run firstParser input
             match firstParserResult with
                | Failure err -> 
                    Failure err 
                | Success(value1,remaining1) ->
                    let secondResult = run secondParser remaining1
                    match secondResult with
                    | Failure errs ->
                        Failure errs
                    | Success(value2, remaining2) ->
                        let combineResult = (value1,value2)
                        Success (combineResult,remaining2)
        Parser innerFn

    //andthen
    let (.>>.) firstParser secondParser =  
        let innerFn input =  
             let firstParserResult = run firstParser input
             match firstParserResult with
                | Failure err -> 
                    Failure err 
                | Success(value1,remaining1) ->
                    let secondResult = run secondParser remaining1
                    match secondResult with
                    | Failure errs ->
                        Failure errs
                    | Success(value2, remaining2) ->
                        let combineResult = (value1,value2)
                        Success (combineResult,remaining2)
        Parser innerFn    

    let orElse firstParser secondParser =  
        let innerFn input =  
             let firstParserResult = run firstParser input
             match firstParserResult with
                | Failure err -> 
                     let secondParserResult = run secondParser input
                     match secondParserResult with
                     | Success (value2, remaining2) -> 
                            Success(value2,remaining2)
                     | Failure err2 ->
                            Failure err2       
                | Success(value1,remaining1) ->
                    Success(value1,remaining1)
        Parser innerFn

    //orThen
    let (<|>) = orElse
 


    let mapP f p = 
        let innerFunc input = 
            let result = run p input
            match result with
            | Failure err ->
                Failure err
            | Success (value1,remaining1) ->
                let funResult = f(value1)
                Success(funResult,remaining1)
        Parser innerFunc
    let (|>>) p f = mapP f p 

    //ignore the result of first parser and return the rest??

    let (>>%) p x =
        p |>> (fun _ -> x)

    let choice listOfParser =
        listOfParser |> List.reduce (<|>)

    let anyOf listOfChars =
        listOfChars
        |> List.map pchar
        |> choice    


    //Get the result of all parsers in a list
    let sequence listOfParsers = 
        let concatResult p1 p2=
            p1 .>>. p2 
            |>> (fun(list1,list2) -> list1 @ list2)   
        listOfParsers
        |> Seq.map (fun p -> p |>> List.singleton)
        |> Seq.reduce concatResult

    let pString str =
        str
        |> Seq.map pchar
        |> sequence
        |>> List.toArray
        |>> String    

    let rec parseZeroOrMore parser input =
        //run the parser with input
        let firstResult = run parser input
        match firstResult with
        | Success (firstValue,remaining1) -> 
                let (subsequentValues,remainingInput) 
                    = parseZeroOrMore parser remaining1
                let values = firstValue :: subsequentValues
                (values,remainingInput)     
        | Failure err ->
            ([],input)

    //matches zero or more occurence of a given parser
    let many parser =
        let rec innerFn input =
            //parse the input - wrap in success as its always successfull
            Success (parseZeroOrMore parser input)
        Parser innerFn

    let many1 parser =
        let rec innerFn input =
            let firstResult = run parser input
            match firstResult with
            | Failure err -> 
                Failure err
            | Success (firstValue1,remainingValue1) ->
                    let (subsequentValues,remainingInput) 
                        = parseZeroOrMore parser remainingValue1
                    let values =  firstValue1 :: subsequentValues
                    Success(values,remainingInput)
        Parser innerFn

    let (.>>) parser1 parser2 =
        let innerFn input = 
            let result = run (parser1 .>>. parser2) input
            match result with
            | Success ((f,s), remaining) -> Success(f,remaining) 
            | Failure err -> Failure err
        Parser innerFn

    let (>>.) parser1 parser2 =
        let innerFn input = 
            let result = run (parser1 .>>. parser2) input
            match result with
            | Success ((f,s), remaining) -> Success(s,remaining) 
            | Failure err -> Failure err
        Parser innerFn    

    let between p1 p2 p3 = p1 >>. p2 .>> p3  

    let digit = anyOf ['0'..'9']
    let digits = many1 digit

    let returnP x = 
        let innerFn input =
            Success(x,input)
        Parser innerFn

    let anychar  =
        let innerFn input = 
            if String.IsNullOrEmpty(input) then
                Failure "No More Input"
            else
               let first = input.[0]
               let remaining = input.[1..]
               Success(first,remaining)
        Parser innerFn 

    let anyString: Parser<string> =
            let resultToString charList = 
                String(List.toArray charList)
            (many1 anychar) |>> resultToString 

    //parse one or more occurences of p seperated by sep
    let sepBy1 p sep = 
        let sepThenP = sep >>. p
        p .>>. many sepThenP
        |>> fun (p, pList) -> p::pList 

    //parse zero or more occurences of p seperated by sep
    let sepBy p sep = 
        sepBy1 p sep <|> returnP []


    let pInt =
        //helper function
        let resultToInt digitList = 
            String(List.toArray digitList) |> int
        //map digits to an int
        digits 
        |>> resultToInt

    let getCharToStringExeptSep exceptCharArry = 
        let validCharsFn  (ch:char) = not (Array.Exists(exceptCharArry, fun c -> ch = c))
        many (satisfy validCharsFn)

    let charsExcept exceptChars =
        let charNot ch carray =   not (Array.Exists(carray, fun c -> ch = c))
        many (satisfy (fun ch -> charNot ch exceptChars))

    let pCharsToStringExcept exceptChars=
        let resultToString chars = 
            String(List.toArray chars) 
        getCharToStringExeptSep  exceptChars
        |>> (List.toArray >> String) 

    //Newly created

    let pStringBeforeString (beforeStr:string)  =
            let innerFn input =
                if String.IsNullOrEmpty(input) then
                    Failure "No More Input..."
                else
                    let index  = input.IndexOf(beforeStr)
                    if (index  >= 0) then
                        Success((input.Substring(0,index)),(input.Substring(index,(input.Length - index)))) 
                    else
                       let msg = sprintf "Expecting %s in %s" beforeStr input
                       Failure msg
            Parser innerFn        

    let getSepByStringParser stringSeparator =
        let stringParserExceptString = pStringBeforeString stringSeparator <|> anyString
        let stringSeperatorParser = pString stringSeparator
        sepBy stringParserExceptString stringSeperatorParser 

    //Concatinate string
    let (^) l r = sprintf "%s%s" l r


    let fieldParser =  (pCharsToStringExcept [|'~';'*'|] .>> anyOf ['~';'*']) <|> (pCharsToStringExcept [|'~';'*'|]) 

    let fieldParserNew = sepBy1 (pCharsToStringExcept [|'~';'*'|])  (anyOf ['~';'*'])
    run fieldParserNew "123aA*BBB" |> ignore

    let allLines = System.IO.File.ReadLines(@"C:\Users\MDHIRAN\source\repos\Parser\Sample835.txt")
    
  
    //https://swlaschin.gitbooks.io/fsharpforfunandprofit/content/posts/understanding-parser-combinators-4.html
    //https://www.youtube.com/watch?v=RDalzi7mhdY&t=1682s
    //https://swlaschin.gitbooks.io/fsharpforfunandprofit/content/posts/understanding-parser-combinators-3.html
    //fParsec - http://www.quanttec.com/fparsec/reference/primitives.html#members.attempt
    //835 sample - https://x12.org/examples/005010x221/example-2-multiple-claims-single-check    
    //835 Tree structure - https://www.xtranslator.com/prod/beginguidex12.pdf
    //Repeating loops ISA-IEE, GS-GE, ST-SE