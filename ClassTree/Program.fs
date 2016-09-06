// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

let getAssembly assemblyPath = System.Reflection.Assembly.LoadFile(assemblyPath)
let getAssemblyClasses (assembly: System.Reflection.Assembly) = assembly.GetTypes()
let printfSystemType (m:System.Reflection.MemberInfo) = printf "%s:%s" m.Name (m.MemberType.ToString())

let rec printClass (c: System.Type )= 
    printf "%s" c.Name
    let systemTypes, customTypes =  c.GetMembers() |> Array.toList |> List.partition ( fun cm -> cm.ReflectedType.Namespace = "System")
    systemTypes |> List.sortBy (fun (x:System.Reflection.MemberInfo) -> x.Name) |> List.iter printfSystemType
    customTypes |> List.sortBy (fun (x:System.Reflection.MemberInfo) -> x.Name) |> List.iter printfSystemType
    printf "End"


[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    getAssembly argv.[0] |> getAssemblyClasses |> Array.iter printClass
    0 // return an integer exit code
