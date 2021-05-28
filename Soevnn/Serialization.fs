module Soevnn.Serialization
open Soevnn.Core
open MessagePack
open MessagePack.FSharp
open MessagePack.Resolvers
open MessagePack.Formatters

type RefFormatter<'T>() =
    interface IMessagePackFormatter<Ref<'T>> with
        member this.Deserialize(reader: byref<MessagePackReader>, options: MessagePackSerializerOptions): Ref<'T> = 
            let resolver = options.Resolver
            options.Security.DepthStep(&reader)
            let value = resolver.GetFormatterWithVerify<'T>().Deserialize(&reader,options)
            reader.Depth <- reader.Depth - 1
            ref value
        member this.Serialize(writer: byref<MessagePackWriter>, value: Ref<'T>, options: MessagePackSerializerOptions): unit = 
            options.Resolver.GetFormatterWithVerify<'T>().Serialize(&writer, value.Value, options)
        

type private Cache<'T> =
    static member Formatter : IMessagePackFormatter<'T> =
        
        if typedefof<'T> = typedefof<Ref<int>> then
            let td = typeof<'T>
            let subt = td.GenericTypeArguments.[0]
            let refformtd = typedefof<RefFormatter<int>>
            let reformt = refformtd.MakeGenericType([|subt|])
            //let reformc = reformt.GetConstructor(System.Type.EmptyTypes)
            
            let rf = System.Activator.CreateInstance(reformt) // reformc.Invoke([||])
            rf :?> IMessagePackFormatter<'T>
        else
            let r =
                Array.tryFind
                    (fun (resolver : IFormatterResolver) ->
                        let f = resolver.GetFormatter<'T>()
                        f <> null)
                    SoevnnResolver.Resolvers
            match r with 
            | Some r -> r.GetFormatter<'T>()
            | _ -> null

and SoevnnResolver() =
    static member Instance = new SoevnnResolver()
    static member Resolvers : IFormatterResolver[] = 
        [|
            FSharpResolver.Instance;
            StandardResolver.Instance;
        |]
    interface IFormatterResolver with
        member this.GetFormatter<'T>(): IMessagePackFormatter<'T> = 
            Cache<'T>.Formatter;
    

    
    



let Resolver =
    SoevnnResolver.Instance

let MessagePackOptions = 
    MessagePackSerializerOptions.Standard.WithResolver(Resolver).WithAllowAssemblyVersionMismatch(true)

let SaveNervousSystem (ns : NervousSystem) (na : (NeuralAddress * Neuron) array) (stream : System.IO.Stream) =
    MessagePackSerializer.Serialize(stream,(ns,na),MessagePackOptions)
     
let LoadNervousSystem (stream : System.IO.Stream) =
    try
        Some <| MessagePackSerializer.Deserialize<NervousSystem * (NeuralAddress * Neuron) array>(stream,MessagePackOptions)
    with
        | :? MessagePackSerializationException as ex -> None
