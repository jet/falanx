namespace Falanx.Ast
module Prelude =
    open System
    open System.IO

    let invalidOpf fmt = Printf.ksprintf invalidOp fmt
    
    /// Concatenates two paths using System.IO.Path.Combine
    let (</>) path1 path2 = Path.Combine(path1, path2)
    
    /// Concatenates two namespaces/class names and separates them with "."
    let (+.+) scope1 scope2 = (scope1 + "." + scope2).Trim('.')
    
    let create<'T when 'T: (new: unit -> 'T)>() = new 'T()
    
    let argNotNull name (value: obj) =
        if isNull value
        then raise <| ArgumentNullException(name)