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

    //type EDIItem = Tree<SegmentInfo,LoopInfo> 
    
    // raw segment record type
    type RawSegmentRecord = {segmentLineNumber:int;segmentName: string; fieldInfoList : FieldInfo[]; isStartLoopSegment:bool;isEndLoopSegment:bool;loopLevel:int option}
    type RawSegmentRecordLoopInfo = {rawSegmentRecord:RawSegmentRecord;loopSequenceNumber:int;loopLevel:int}
    //type RawSegmentRecordWithLoopIdentifier = {isLoop: bool;segmentRecord: RawSegmentRecord;loopSequenceNumber: int option;loopLevel: int option}
    type RawSegmentRecordWithLoopIdentifier =
            | RawSegmentRecordInfo of RawSegmentRecord
            | LoopWithIndexRecordInfo of RawSegmentRecordLoopInfo
    
    //Helper types
    type LoopIndexValue = {loopName:string;index:int}
    type LoopIndexValueList = LoopIndexValue list 

    //meta info of loop
    type LoopMetaInfo = {loopFirstField: string;loopEndField: string;loopLevel: int;}
    
    //Directory i.e. Loop 
    type Directory = RawSegmentRecordWithLoopIdentifier list
    type DirectoryWithDirectoryInfo = {directoryLoopInfo: RawSegmentRecordLoopInfo; directoryContent: RawSegmentRecordWithLoopIdentifier list; directorySegments:RawSegmentRecordWithLoopIdentifier list}

    //EDI Start End Loop Meta info
    type EDIStartEndLoopInfo = {startLoopInfo:IDictionary<string,LoopMetaInfo>;endLoopInfo:IDictionary<string,LoopMetaInfo>} 

    (*let getDirectoryInfoOld (list:RawSegmentRecordWithLoopIdentifier list) =
        match list with
            | h:: t -> match h with
                        | LoopWithIndexRecordInfo l -> Some ({directoryLoopInfo=l;directoryContent=list})
                        | _ -> None
            | _ -> None*)

    let getTheFirstLoppAndOnlySegmentsBeforeNextLoop (list:RawSegmentRecordWithLoopIdentifier list) =
        list.Head :: List.takeWhile (fun c -> match c with 
                                    | RawSegmentRecordInfo r -> true
                                    | LoopWithIndexRecordInfo l -> false    
                       ) list.Tail
    //take from the list only the segment and the first Loop -- other segments will be part of other directory
    let getDirectoryInfo (list:RawSegmentRecordWithLoopIdentifier list) =
        let onlydirectorySegments = getTheFirstLoppAndOnlySegmentsBeforeNextLoop list
        match onlydirectorySegments with
            | h:: t -> match h with
                        | LoopWithIndexRecordInfo l -> Some ({directoryLoopInfo=l;directoryContent=list;directorySegments=onlydirectorySegments})
                        | _ -> None
            | _ -> None
   
    //835 Meta Loop Info
    let EDI835LoopInfo = 
        dict 
            [ 
                ("ISA",{ loopFirstField = "ISA"; loopEndField= "ISE";loopLevel=1});
                ("GS",{ loopFirstField = "GS"; loopEndField= "GE";loopLevel=2});
                ("ST",{ loopFirstField= "ST"; loopEndField= "SE";loopLevel=3});
                ("LX",{ loopFirstField = "LX"; loopEndField= "LQ";loopLevel=4});
                ("CLP",{ loopFirstField= "CLP"; loopEndField= "CLP";loopLevel=5});
            ]

    let EDI835EndLoopInfo = 
        dict 
            [ 
                ("IEA",{ loopFirstField = "ISA"; loopEndField= "ISE";loopLevel=1});
                ("GE",{ loopFirstField = "GS"; loopEndField= "GE";loopLevel=2});
                ("SE",{ loopFirstField= "ST"; loopEndField= "SE";loopLevel=3});
                ("LQ",{ loopFirstField = "LX"; loopEndField= "LQ";loopLevel=4});
            ]

    
    let EDI835StartEndLoopInfo = {startLoopInfo=EDI835LoopInfo;endLoopInfo=EDI835EndLoopInfo}

    let getFieldInfoFromStringArray (segmentName:string) (fieldStrings:string[]) =
        Array.mapi (fun i f -> {fieldSequenceNumber=i;fieldName=segmentName ^ i.ToString() ;fieldValue=f}) fieldStrings

    let getRawSegmentRecordData (ediStartEndLoopInfo:EDIStartEndLoopInfo) (segmentsWithFields: string[][]) =
        Array.mapi (
                fun (index:int) (f:string[]) -> 
                let segmentName = f.[0]
                let isStartLoop = ediStartEndLoopInfo.startLoopInfo.ContainsKey(segmentName)
                let isEndLoop = ediStartEndLoopInfo.endLoopInfo.ContainsKey(segmentName);
                let loopLevel = if isStartLoop then 
                                    Some(ediStartEndLoopInfo.startLoopInfo.Item(segmentName).loopLevel) 
                                else if isEndLoop then 
                                    Some (ediStartEndLoopInfo.endLoopInfo.Item(segmentName).loopLevel) 
                                else None
                {segmentLineNumber=index; segmentName = f.[0];fieldInfoList = getFieldInfoFromStringArray f.[0] (Array.skip 1 f); isStartLoopSegment= isStartLoop;isEndLoopSegment= isEndLoop;loopLevel=loopLevel}
            )
            segmentsWithFields

    let getNewDicFromOrigDic  (orignalDic: LoopIndexValueList) (kv:LoopIndexValue) =
        let retVal = List.filter (fun v -> v.loopName <> kv.loopName) orignalDic
        kv :: retVal

    let getRawSegmentWithLoopIdentifier (rawSegmentRecord:RawSegmentRecord) (loopInfoDic: IDictionary<string,LoopMetaInfo>) (loopIndexDic: LoopIndexValueList) =
        let isLoop = loopInfoDic.ContainsKey(rawSegmentRecord.segmentName)
        let loopLevel = match isLoop with 
                            | true -> Some(loopInfoDic.Item rawSegmentRecord.segmentName |> fun f -> f.loopLevel)
                            | false -> None
        let (loopIndexValue: LoopIndexValue option) = List.tryFind (fun (c:LoopIndexValue) -> c.loopName = rawSegmentRecord.segmentName) loopIndexDic 
        let newIndexDic =  match (isLoop,loopIndexValue) with
                                    | true, Some value -> 
                                                        let currentLoopIndex = value.index + 1
                                                        let newDic = getNewDicFromOrigDic loopIndexDic {loopName= value.loopName;index=currentLoopIndex + 1}      
                                                        newDic
                                    | true, _ -> getNewDicFromOrigDic loopIndexDic {loopName= rawSegmentRecord.segmentName;index=1}
                                    | false, _ -> loopIndexDic
        let indexValueToBeAssigned = match isLoop with 
                                            | true -> match List.tryFind (fun (c:LoopIndexValue) -> c.loopName = rawSegmentRecord.segmentName) newIndexDic with
                                                        | Some xx -> Some(xx.index)
                                                        | _ -> None
                                            | false -> None
        
        let segmentWithLoopIdentifier = match isLoop with
                                            | true -> LoopWithIndexRecordInfo {rawSegmentRecord=rawSegmentRecord;loopSequenceNumber= indexValueToBeAssigned.Value;loopLevel= loopLevel.Value}
                                            | false -> RawSegmentRecordInfo rawSegmentRecord
        (segmentWithLoopIdentifier,newIndexDic)

    let foldFun (s:(RawSegmentRecordWithLoopIdentifier list*LoopIndexValueList)) (record:RawSegmentRecord) =
            let stateList,dic = s
            let (x,y) = getRawSegmentWithLoopIdentifier record EDI835LoopInfo dic
            let newStateList = x::stateList
            (newStateList,y)

    let getSegmentsWithLoopIdentifier (records:RawSegmentRecord[]) =
        let loopIndexDic: LoopIndexValueList = []
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
            let rawSegmentData = getRawSegmentRecordData EDI835StartEndLoopInfo segmentWithFields
            let segmentWithLoopIdentifier = getSegmentsWithLoopIdentifier rawSegmentData |> List.rev
            segmentWithLoopIdentifier
    

    //Create Tree of type EDIItem = Tree<SegmentInfo,LoopInfo> 
    //type SegmentInfo = {segmentName: string; fields:FieldInfo[]}
    //type LoopInfo = {loopName: string;fields:FieldInfo[]} 
    //type RawSegmentRecord = {segmentName: string; fieldInfoList : FieldInfo[]}
    //type RawSegmentRecordWithLoopIdentifier = {isLoop: bool;segmentRecord: RawSegmentRecord;loopSequenceNumber: int option;loopLevel: int option}
    //type Directory = RawSegmentRecordWithLoopIdentifier list

    let fromFile (s:RawSegmentRecord) = LeafNode {segmentName=s.segmentName;fields=s.fieldInfoList} 
    
    let enumerateFiles (d:DirectoryWithDirectoryInfo) =
        let seg = List.takeWhile(fun (f:RawSegmentRecordWithLoopIdentifier) ->  match f with | RawSegmentRecordInfo _ -> true | _ -> false  ) d.directoryContent.Tail
        let onlyFiles = List.fold (fun s t-> 
                           match t with 
                            | RawSegmentRecordInfo r -> r::s
                            | _ -> s) [] seg  
        onlyFiles

    let getSegmentSequenceNumber item =
            match item with
                | LoopWithIndexRecordInfo c -> c.rawSegmentRecord.segmentLineNumber
                | RawSegmentRecordInfo r -> r.segmentLineNumber

    let rec takeDirectoryAtLevel (level:int) (list: RawSegmentRecordWithLoopIdentifier list) (agg:DirectoryWithDirectoryInfo list)=
            match list with
               | h::t -> 
                        let rest = List.skipWhile (fun f -> match f with 
                                                                | LoopWithIndexRecordInfo c -> c.loopLevel < level
                                                                | RawSegmentRecordInfo _ -> true    
                                                  ) list
                        if rest.IsEmpty = false && rest.Length > 1 then 
                            let headLooprecord = List.head rest  
                            let headLoopRecordSegmentNumber = getSegmentSequenceNumber headLooprecord
                            
                            //Now take from rest till next LoopWithIndexRecordInfo at same level is found -- that will be one directory - 
                            //while comparing c.loopLevel = level need to skip the head record in tail
                            //Skip till first LoopWithIndexRecordInfo record whose loopLevel = level is not found and from there onwards we can
                            let d = List.takeWhile (fun f -> match f with 
                                                                | LoopWithIndexRecordInfo c -> c.loopLevel > level || (c.loopLevel = level &&  c.rawSegmentRecord.segmentLineNumber = headLoopRecordSegmentNumber)
                                                                | RawSegmentRecordInfo r -> r.isEndLoopSegment = false || (r.isEndLoopSegment = true && r.loopLevel.Value >= level)    
                                                    ) rest
            
                            let dd = getDirectoryInfo  (d)
                            //takentill
                            let lastElement = List.last d
                            let lastSegmentLineNumber =  getSegmentSequenceNumber lastElement
                            let newAgg = match dd with
                                            | Some ddd -> ddd :: agg
                                            | None -> agg
                            //Need to convert d to DirectoryWithDirectoryInfo
                            (*
                            let remaning = List.skipWhile (fun f -> match f with 
                                                                        | LoopWithIndexRecordInfo c -> c.loopLevel >= level
                                                                        | RawSegmentRecordInfo _ -> true    
                                                           ) reatRest.Tail*)
                            let remaning = List.skipWhile (fun f -> match f with 
                                                                        | LoopWithIndexRecordInfo c -> c.rawSegmentRecord.segmentLineNumber <= lastSegmentLineNumber
                                                                        | RawSegmentRecordInfo r -> r.segmentLineNumber <= lastSegmentLineNumber
                                                           ) rest
                            takeDirectoryAtLevel level remaning newAgg
                        else
                            agg
                | _ -> agg
        
    //this will return the directories which are immetiately below current directory
    let enumerateDictionaries (d:DirectoryWithDirectoryInfo) =
        let dirList = takeDirectoryAtLevel (d.directoryLoopInfo.loopLevel + 1) d.directoryContent []
        dirList
        
    let rec fromDir (dirInfo:DirectoryWithDirectoryInfo) = 
        let subItems = seq{
            yield! enumerateFiles dirInfo |> Seq.map fromFile
            yield! enumerateDictionaries dirInfo |> Seq.map fromDir
            }
        InternalNode (dirInfo,subItems)

    let getTree ediString =
            let segmentsWithLoopIdentfierList = getSegmentsWithLoopIdentifierFromEDIContent ediString
            let dirInfoOption = getDirectoryInfo segmentsWithLoopIdentfierList
            match dirInfoOption with
                | Some dirInfo -> Some (fromDir dirInfo)
                | _ -> None

    //iterate   
    //let rec fromDir (dirInfo:DirectoryInfo) = 
    //    let subItems = seq{
    //        yield! dirInfo.EnumerateFiles() |> Seq.map fromFile
    //        yield! dirInfo.EnumerateDirectories() |> Seq.map fromDir
    //        }
    //    InternalNode (dirInfo,subItems)
    
    //https://swlaschin.gitbooks.io/fsharpforfunandprofit/content/posts/recursive-types-and-folds-3b.html

    

    let hello name =
        printfn "Hello %s" name
