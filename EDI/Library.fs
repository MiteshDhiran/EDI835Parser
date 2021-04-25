namespace EDI

open System

module EDIModule =
    open ParserLib.ParserModule
    open System.Collections.Generic

    //Generic tree code
    type Tree<'LeafData,'INodeData> =
        | LeafNode of 'LeafData
        | InternalNode of 'INodeData * Tree<'LeafData,'INodeData> seq

    let rec cata fLeaf fNode (tree:Tree<'LeafData,'INodeData>) :'r = 
        let recurse = cata fLeaf fNode  
        match tree with
        | LeafNode leafInfo -> 
            fLeaf leafInfo 
        | InternalNode (nodeInfo,subtrees) -> 
            fNode nodeInfo (subtrees |> Seq.map recurse)

    let rec fold fLeaf fNode acc (tree:Tree<'LeafData,'INodeData>) :'r = 
        let recurse = fold fLeaf fNode  
        match tree with
        | LeafNode leafInfo -> 
            fLeaf acc leafInfo 
        | InternalNode (nodeInfo,subtrees) -> 
            // determine the local accumulator at this level
            let localAccum = fNode acc nodeInfo
            // thread the local accumulator through all the subitems using Seq.fold
            let finalAccum = subtrees |> Seq.fold recurse localAccum 
            // ... and return it
            finalAccum

    //EDI Item Type definition
    type FieldInfo = {fieldSequenceNumber: int;fieldName:string; fieldValue: string;}
    type SegmentInfo = {segmentName: string; fields:FieldInfo[]}
    type LoopInfo = {loopName: string;fields:FieldInfo[]}

    type EDIItem = Tree<SegmentInfo,LoopInfo> 
    
    // raw segment record type
    type RawSegmentRecord = {segmentName: string; fieldInfoList : FieldInfo[]}
    type RawSegmentRecordWithLoopIdentifier = {isLoop: bool;segmentRecord: RawSegmentRecord;loopSequenceNumber: int option;loopLevel: int option}

    //meta info of loop
    type LoopMetaInfo = {loopFirstField: string;loopEndField: string;loopLevel: int;}

    type LoopIndexValue = {loopName:string;index:int}

    type LoopIndexValueList = LoopIndexValue list 

    //835 Meta Loop Info
    let EDI835LoopInfo = 
        dict 
            [ 
                ("ISA",{ loopFirstField = "ISA"; loopEndField= "ISE";loopLevel=1});
                ("GS",{ loopFirstField = "GS"; loopEndField= "GE";loopLevel=2});
                ("ST",{ loopFirstField= "ST"; loopEndField= "SE";loopLevel=3});
                ("LX",{ loopFirstField = "LX"; loopEndField= "LQ";loopLevel=4});
                ("CLP",{ loopFirstField= "CLP"; loopEndField= "CLP";loopLevel=4});
            ]

    let getFieldInfoFromStringArray (segmentName:string) (fieldStrings:string[]) =
        Array.mapi (fun i f -> {fieldSequenceNumber=i;fieldName=segmentName ^ i.ToString() ;fieldValue=f}) fieldStrings

    let getRawSegmentRecordData (segmentsWithFields: string[][]) =
        Array.map (fun (f:string[]) -> {segmentName = f.[0];fieldInfoList = getFieldInfoFromStringArray f.[0] (Array.skip 1 f)}) segmentsWithFields

    let getNewDicFromOrigDic  (orignalDic: LoopIndexValueList) (kv:LoopIndexValue) =
        let retVal = List.filter (fun v -> v.loopName <> kv.loopName) orignalDic
        kv :: retVal

    let getRawSegmentWithLoopIdentifier (rawSegmentRecord:RawSegmentRecord) (loopInfoDic: IDictionary<string,LoopMetaInfo>) (loopIndexDic: LoopIndexValueList) =
        let isLoop = loopInfoDic.ContainsKey(rawSegmentRecord.segmentName)
        let loopLevel = match isLoop with 
                            | true -> Some(loopInfoDic.Item rawSegmentRecord.segmentName |> fun f -> f.loopLevel)
                            | false -> None
        let (loopIndexValue: LoopIndexValue option) = List.tryFind (fun (c:LoopIndexValue) -> c.loopName = rawSegmentRecord.segmentName) loopIndexDic 
        let newIndexValue =  match (isLoop,loopIndexValue) with
                                    | true, Some value -> 
                                                        let currentLoopIndex = value.index + 1
                                                        let newDic = getNewDicFromOrigDic loopIndexDic {loopName= value.loopName;index=currentLoopIndex + 1}      
                                                        newDic
                                    | true, _ -> getNewDicFromOrigDic loopIndexDic {loopName= rawSegmentRecord.segmentName;index=1}
                                    | false, _ -> loopIndexDic
                    
        let segmentWithLoopIdentifier = {isLoop= loopInfoDic.ContainsKey(rawSegmentRecord.segmentName);segmentRecord = rawSegmentRecord;loopSequenceNumber = Some(1);loopLevel= loopLevel}
        (segmentWithLoopIdentifier,newIndexValue)

    let foldFun (s:(RawSegmentRecordWithLoopIdentifier list*LoopIndexValueList)) (record:RawSegmentRecord) =
            let stateList,dic = s
            let (x,y) = getRawSegmentWithLoopIdentifier record EDI835LoopInfo dic
            let newStateList = x::stateList
            (newStateList,y)

    let getSegmentsWithLoopIdentifier (records:RawSegmentRecord[]) =
        let loopIndexDic: LoopIndexValueList = []
        let mutable segmentWithLoopIdentifierList : RawSegmentRecordWithLoopIdentifier list = []
        let resultList, dic = Array.fold ( foldFun) ([],loopIndexDic) records
        resultList 
        
        //Array.map (fun record -> getRawSegmentWithLoopIdentifier record EDI835LoopInfo loopIndexDic) records
        
    let getSegmentToFields segmentString =
        let segmentToFieldList = run (getSepByStringParser "*") segmentString
        match segmentToFieldList with
                | Success (l,_) -> l |> List.toArray
                | Failure f -> [||]

    let getSegmentsWithLoopIdentifierFromEDIContent ediString = 
            let segmentResult = run (getSepByStringParser Environment.NewLine) ediString
            let segments =  match segmentResult with
                                | Success (l,_) -> l |> List.toArray
                                |  Failure f -> [||]
            let segmentWithFields = Array.map (fun segLine -> getSegmentToFields segLine) segments
            let rawSegmentData = getRawSegmentRecordData segmentWithFields
            let segmentWithLoopIdentifier = getSegmentsWithLoopIdentifier rawSegmentData
            segmentWithLoopIdentifier

    //iterate
    //let rec fromDir (dirInfo:DirectoryInfo) = 
    //    let subItems = seq{
    //        yield! dirInfo.EnumerateFiles() |> Seq.map fromFile
    //        yield! dirInfo.EnumerateDirectories() |> Seq.map fromDir
    //        }
    //    InternalNode (dirInfo,subItems)
    
    

    

    

    let hello name =
        printfn "Hello %s" name
