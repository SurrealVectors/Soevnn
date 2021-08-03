module Soevnn.Serialization
open Soevnn.Core
open MessagePack
open MessagePack.FSharp
open MessagePack.Resolvers
open MessagePack.Formatters

// Pretty sure this doesn't properly deserialize multiple references to the same value.
// This doesn't currently matter as any such cases are instead referenced in a system agnostic manner via mechanisms such as NeuralAddress.
// Should probably fix this at some point anyways.
///  <summary> Is a formatter for the Ref type. Provides methods for serializing and deserializing the Ref type. </summary>
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
        
/// <summary> A cache of formatters to be used for serialization/deserialization. </summary>
type private Cache<'T> =
    static member Formatter : IMessagePackFormatter<'T> =
        
        if typedefof<'T> = typedefof<Ref<int>> then // Handle the special case of reference cells when applicable.
            let td = typeof<'T>
            let subt = td.GenericTypeArguments.[0] // Get the type parameter.
            let refformtd = typedefof<RefFormatter<int>>
            let reformt = refformtd.MakeGenericType([|subt|]) // Create a RefFormatter with the same type parameter.
            //let reformc = reformt.GetConstructor(System.Type.EmptyTypes)
            
            let rf = System.Activator.CreateInstance(reformt) // reformc.Invoke([||]) // Instantiate the RefFormatter.
            rf :?> IMessagePackFormatter<'T> // Cast to the return type.
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

/// <summary> The resolver to be used for Soevnn. </summary>
and SoevnnResolver() =
    static member Instance = new SoevnnResolver() // Provides the instance of the resolver.
    static member Resolvers : IFormatterResolver[] = // Provides the instances of the base resolvers.
        [|
            FSharpResolver.Instance;
            StandardResolver.Instance;
        |]
    interface IFormatterResolver with
        member this.GetFormatter<'T>(): IMessagePackFormatter<'T> = 
            Cache<'T>.Formatter;
    

    
    


// The instanced resolver.
let Resolver =
    SoevnnResolver.Instance

// The options for serialization/deserialization. 
let MessagePackOptions = 
    MessagePackSerializerOptions.Standard.WithResolver(Resolver).WithAllowAssemblyVersionMismatch(true)

/// <summary> Saves a nervous system. </summary>
let SaveNervousSystem (ns : NervousSystem) (na : (NeuralAddress * Neuron) array) (stream : System.IO.Stream) =
    MessagePackSerializer.Serialize(stream,(ns,na),MessagePackOptions)
     
/// <summary> Tries to load a nervous system. </summary>
let LoadNervousSystem (stream : System.IO.Stream) =
    try
        Some <| MessagePackSerializer.Deserialize<NervousSystem * (NeuralAddress * Neuron) array>(stream,MessagePackOptions)
    with
        | :? MessagePackSerializationException as ex -> None
