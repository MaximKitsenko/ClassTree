// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

let getAssembly assemblyPath = System.Reflection.Assembly.LoadFile(assemblyPath)
let getAssemblyClasses (assembly: System.Reflection.Assembly) = assembly.GetTypes()
let printfMemberOfSystemType (m:System.Reflection.MemberInfo) = printfn "%s:%s" m.Name (m.MemberType.ToString())

let rec printfCustomType (m:System.Reflection.MemberInfo) = 
    printfn "%s:[CustomType]" m.Name 
    printClass (m.MemberType.GetType())

and printClass (c: System.Type )= 
    printfn "%s" c.Name
    let systemTypes, customTypes =  c.GetMembers() |> Array.toList |> List.partition ( fun cm -> cm.ReflectedType.Namespace = "System")
    systemTypes |> List.sortBy (fun (x:System.Reflection.MemberInfo) -> x.Name) |> List.iter printfMemberOfSystemType
    customTypes |> List.sortBy (fun (x:System.Reflection.MemberInfo) -> x.Name) |> List.iter printfCustomType

[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    getAssembly argv.[0] |> getAssemblyClasses |> Array.iter printClass
    0 // return an integer exit code
