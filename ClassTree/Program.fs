// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
type fieldType = ArrayOf | SystemType | CustomType 


let getAssembly assemblyPath = System.Reflection.Assembly.LoadFile(assemblyPath)
let getAssemblyClasses (assembly: System.Reflection.Assembly) = assembly.GetTypes()
let printStringWithTabs t s = [0..t]|> List.map (fun x -> "\t") |> (@) [s] |> List.rev|> (@) ["\n"] |> List.iter (fun x-> printf "%s" x)
let printfMemberOfSystemType t (m:System.Reflection.FieldInfo) = printStringWithTabs t (sprintf "%s : %s" m.Name (m.FieldType.ToString()))
let getFieldInfoWithNotes (fi:System.Reflection.FieldInfo)= 
    if fi.FieldType.IsArray then
        (fi, fieldType.ArrayOf, fi.FieldType.GetElementType())
    elif fi.FieldType.Namespace = "System"
        (fi, fieldType.SystemType, fi.FieldType.GetElementType())
    else
        (fi, fieldType.CustomType, fi.FieldType.GetElementType())
 
let rec printfCustomType (deep:int) (m:System.Reflection.MemberInfo) = 
    printStringWithTabs deep m.Name 
    printClass (deep+1) (m.MemberType.GetType())

and printClass (deep:int) (c: System.Type )= 
    printStringWithTabs deep c.Name  
    let temp = c.GetFields( System.Reflection.BindingFlags.NonPublic ||| System.Reflection.BindingFlags.Instance )|> Array.filter (fun x -> (x.MemberType.ToString()) = "Field" && (x.Name.EndsWith("Field")) ) |> Array.toList
    let systemTypes, customTypes =  temp|> List.partition ( fun cm -> cm.FieldType.Namespace = "System")
    let customGenericTypes, customSimpleTypes =  customTypes |> List.partition ( fun cm -> ( cm.FieldType.BaseType.ToString() <> "System.Object" ) )
    systemTypes |> List.sortBy (fun x -> x.Name) |> List.iter (printfMemberOfSystemType (deep+1))
    customSimpleTypes |> List.sortBy (fun x -> x.Name) |> List.iter (printfCustomType (deep+1))
    customGenericTypes |> List.sortBy (fun x -> x.Name) |> List.iter (printfCustomType (deep+1))

[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    getAssembly argv.[0] |> getAssemblyClasses |> Array.filter (fun x -> x.Name = argv.[1]) |> Array.iter (printClass 0)
    0 // return an integer exit code
