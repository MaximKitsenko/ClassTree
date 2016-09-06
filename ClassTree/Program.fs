// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
type fieldType = ArrayOf | SystemType | CustomType 

let getAssembly assemblyPath = System.Reflection.Assembly.LoadFile(assemblyPath)
let getAssemblyClasses (assembly: System.Reflection.Assembly) = assembly.GetTypes()

let printStringWithTabs t s = [0..t]|> List.map (fun x -> "\t") |> (@) [s] |> List.rev|> (@) ["\n"] |> List.iter (fun x-> printf "%s" x)
let printfMemberOfSystemType t (m:System.Reflection.FieldInfo) = printStringWithTabs t (sprintf "%s : %s" m.Name (m.FieldType.ToString()))

let getFieldInfoWithNotes (fi:System.Reflection.FieldInfo)= 
    if fi.FieldType.IsArray then
        ( fieldType.ArrayOf,fi, fi.FieldType.GetElementType())
    elif fi.FieldType.Namespace = "System" then
        ( fieldType.SystemType,fi, fi.FieldType.GetElementType())
    else 
        ( fieldType.CustomType,fi, fi.FieldType.GetElementType())

let rec printClassMember (deep: int) cm = 
    match cm with
    | (fieldType.SystemType,fi,st) -> printfMemberOfSystemType ( deep + 1) fi
    | (fieldType.CustomType,fi,st) -> printStringWithTabs deep fi.Name 
                                      printClass (deep+1) (fi.MemberType.GetType())
    | (fieldType.ArrayOf,fi,st) -> printStringWithTabs deep <| sprintf "%s[]" (st.ToString())
                                   printClass (deep+1) (st)

and printClass (deep:int) (c: System.Type ) = 
    printStringWithTabs deep c.Name  
    let temp = c.GetFields( System.Reflection.BindingFlags.NonPublic ||| System.Reflection.BindingFlags.Instance ) |> Array.filter (fun x -> (x.MemberType.ToString()) = "Field" && (x.Name.EndsWith("Field")) ) |> Array.toList |> List.map getFieldInfoWithNotes |> List.sortBy (fun (x,_,_)-> x) |> List.iter (fun x-> (printClassMember deep) x)
    ()

[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    getAssembly argv.[0] |> getAssemblyClasses |> Array.filter (fun x -> x.Name = argv.[1]) |> Array.iter (printClass 0)
    0 // return an integer exit code