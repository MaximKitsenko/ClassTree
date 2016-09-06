// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
type fieldType = ArrayOf | SystemType | CustomType 
let printStringWithTabs t s = [1..t]|> List.map (fun x-> "\t") |> List.fold (+) "" |> fun x-> ( x + s) |> (fun x-> printfn "%s" x) // lymda 'fun x-> ( x + s)' can be replaced with 'let inline flip f x y = f y x'
let getFieldInfoWithNotes (fi:System.Reflection.FieldInfo) = if fi.FieldType.IsArray then ( fieldType.ArrayOf,fi, fi.FieldType.GetElementType()) elif fi.FieldType.Namespace = "System" then ( fieldType.SystemType,fi, fi.FieldType.GetElementType()) else ( fieldType.CustomType,fi, fi.FieldType.GetElementType())

let rec printClassMember (deep: int) (cm:fieldType, fi: System.Reflection.FieldInfo, st:System.Type) = 
    match (cm, fi, st) with
    | (fieldType.SystemType,fi,st) -> printStringWithTabs ( deep + 1) ( fi.Name + " : " + (fi.FieldType.ToString()) ) 
    | (fieldType.CustomType,fi,st) -> printStringWithTabs deep fi.Name 
                                      printClass (deep+1) (fi.MemberType.GetType())
    | (fieldType.ArrayOf,fi,st) -> printStringWithTabs ( deep + 1) <|  (sprintf "%s : %s[]" fi.Name (st.ToString()))
                                   printClass (deep+2) (st)
and printClass (deep:int) (c: System.Type ) = 
    printStringWithTabs deep c.Name  
    let temp = c.GetFields( System.Reflection.BindingFlags.NonPublic ||| System.Reflection.BindingFlags.Instance ) |> Array.filter (fun x -> (x.MemberType.ToString()) = "Field" && (x.Name.EndsWith("Field")) ) |> Array.toList |> List.map getFieldInfoWithNotes |> List.sortBy (fun (_,x,_)-> x.Name) |> List.iter (fun x-> (printClassMember deep) x)
    ()

[<EntryPoint>]
let main argv = 
    let classes = (System.Reflection.Assembly.LoadFile( argv.[0] ).GetTypes()) |> Array.filter (fun x -> x.Name = argv.[1] && x.Namespace = argv.[2]) |> Array.iter (printClass 0)
    System.Console.ReadLine() |> ignore
    0 // return an integer exit code