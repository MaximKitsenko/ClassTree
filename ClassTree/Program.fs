// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

let getAssembly assemblyPath = System.Reflection.Assembly.LoadFile(assemblyPath)
let getAssemblyClasses (assembly: System.Reflection.Assembly) = assembly.GetTypes()
let printfMemberOfSystemType (m:System.Reflection.FieldInfo) = printfn "%s:%s" m.Name (m.FieldType.ToString())

let rec printfCustomType (m:System.Reflection.MemberInfo) = 
    printfn "%s:[CustomType]" m.Name 
    printClass (m.MemberType.GetType())

and printClass (c: System.Type )= 
    printfn "%s" c.Name  
    let temp = c.GetFields( System.Reflection.BindingFlags.NonPublic ||| System.Reflection.BindingFlags.Instance )|> Array.filter (fun x -> (x.MemberType.ToString()) = "Field" ) |> Array.toList
    let systemTypes, customTypes =  temp|> List.partition ( fun cm -> cm.FieldType.Namespace = "System")
    systemTypes |> List.sortBy (fun x -> x.Name) |> List.iter printfMemberOfSystemType
    customTypes |> List.sortBy (fun x -> x.Name) |> List.iter printfCustomType

[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    getAssembly argv.[0] |> getAssemblyClasses |> Array.filter (fun x -> x.Name = argv.[1]) |> Array.iter printClass
    0 // return an integer exit code
