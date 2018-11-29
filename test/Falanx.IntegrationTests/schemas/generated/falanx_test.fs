namespace rec protobuf_unittest3

open System
open System.Collections.Generic
open Froto.Serialization
open Falanx.Proto.Codec.Binary
open Falanx.Proto.Codec.Binary.Primitives
open Newtonsoft.Json.Linq
open Fleece.Newtonsoft

[<CLIMutable>]
type TestAllTypes =
    { mutable singleInt32 : int option
      mutable singleInt64 : int64 option
      mutable singleUint32 : UInt32 option
      mutable singleUint64 : UInt64 option
      mutable singleSint32 : int option
      mutable singleSint64 : int64 option
      mutable singleFixed32 : UInt32 option
      mutable singleFixed64 : UInt64 option
      mutable singleSfixed32 : int option
      mutable singleSfixed64 : int64 option
      mutable singleFloat : float32 option
      mutable singleDouble : float option
      mutable singleBool : bool option
      mutable singleString : string option
      mutable repeatedInt32 : int ResizeArray
      mutable repeatedInt64 : int64 ResizeArray
      mutable repeatedUint32 : UInt32 ResizeArray
      mutable repeatedUint64 : UInt64 ResizeArray
      mutable repeatedSint32 : int ResizeArray
      mutable repeatedSint64 : int64 ResizeArray
      mutable repeatedFixed32 : UInt32 ResizeArray
      mutable repeatedFixed64 : UInt64 ResizeArray
      mutable repeatedSfixed32 : int ResizeArray
      mutable repeatedSfixed64 : int64 ResizeArray
      mutable repeatedFloat : float32 ResizeArray
      mutable repeatedDouble : float ResizeArray
      mutable repeatedBool : bool ResizeArray
      mutable repeatedString : string ResizeArray }
    
    static member JsonObjCodec =
        fun singleInt32 singleInt64 singleUint32 singleUint64 singleSint32 singleSint64 singleFixed32 singleFixed64 singleSfixed32 singleSfixed64 singleFloat singleDouble singleBool singleString repeatedInt32 repeatedInt64 repeatedUint32 repeatedUint64 repeatedSint32 repeatedSint64 repeatedFixed32 repeatedFixed64 repeatedSfixed32 repeatedSfixed64 repeatedFloat repeatedDouble repeatedBool repeatedString -> 
            { singleInt32 = singleInt32
              singleInt64 = singleInt64
              singleUint32 = singleUint32
              singleUint64 = singleUint64
              singleSint32 = singleSint32
              singleSint64 = singleSint64
              singleFixed32 = singleFixed32
              singleFixed64 = singleFixed64
              singleSfixed32 = singleSfixed32
              singleSfixed64 = singleSfixed64
              singleFloat = singleFloat
              singleDouble = singleDouble
              singleBool = singleBool
              singleString = singleString
              repeatedInt32 = repeatedInt32
              repeatedInt64 = repeatedInt64
              repeatedUint32 = repeatedUint32
              repeatedUint64 = repeatedUint64
              repeatedSint32 = repeatedSint32
              repeatedSint64 = repeatedSint64
              repeatedFixed32 = repeatedFixed32
              repeatedFixed64 = repeatedFixed64
              repeatedSfixed32 = repeatedSfixed32
              repeatedSfixed64 = repeatedSfixed64
              repeatedFloat = repeatedFloat
              repeatedDouble = repeatedDouble
              repeatedBool = repeatedBool
              repeatedString = repeatedString } : TestAllTypes
        |> fun f -> 
            Fleece.Newtonsoft.withFields<Option<Int32> -> Option<Int64> -> Option<UInt32> -> Option<UInt64> -> Option<Int32> -> Option<Int64> -> Option<UInt32> -> Option<UInt64> -> Option<Int32> -> Option<Int64> -> Option<Single> -> Option<Double> -> Option<Boolean> -> Option<String> -> List<Int32> -> List<Int64> -> List<UInt32> -> List<UInt64> -> List<Int32> -> List<Int64> -> List<UInt32> -> List<UInt64> -> List<Int32> -> List<Int64> -> List<Single> -> List<Double> -> List<Boolean> -> List<String> -> TestAllTypes, IReadOnlyDictionary<String, JToken>, String, TestAllTypes, String, JToken> 
                (f)
        |> fun codec -> 
            let decode =
                let _bind_8a5608f0b53d4652880eecceab3d903b, _ = codec
                _bind_8a5608f0b53d4652880eecceab3d903b
            
            let encode =
                let _, _bind_231ad66b81554ac699140c178630b659 = codec
                _bind_231ad66b81554ac699140c178630b659
            
            Fleece.Newtonsoft.jfieldOpt<TestAllTypes, Int32, Option<Int64> -> Option<UInt32> -> Option<UInt64> -> Option<Int32> -> Option<Int64> -> Option<UInt32> -> Option<UInt64> -> Option<Int32> -> Option<Int64> -> Option<Single> -> Option<Double> -> Option<Boolean> -> Option<String> -> List<Int32> -> List<Int64> -> List<UInt32> -> List<UInt64> -> List<Int32> -> List<Int64> -> List<UInt32> -> List<UInt64> -> List<Int32> -> List<Int64> -> List<Single> -> List<Double> -> List<Boolean> -> List<String> -> TestAllTypes> 
                ("singleInt32") (fun x -> x.singleInt32) (decode, encode)
        |> fun codec -> 
            let decode =
                let _bind_e45894a43e6b430bbd5ef237caaceed4, _ = codec
                _bind_e45894a43e6b430bbd5ef237caaceed4
            
            let encode =
                let _, _bind_3e9f746c55774fa88e5689cf3f6d0d36 = codec
                _bind_3e9f746c55774fa88e5689cf3f6d0d36
            
            Fleece.Newtonsoft.jfieldOpt<TestAllTypes, Int64, Option<UInt32> -> Option<UInt64> -> Option<Int32> -> Option<Int64> -> Option<UInt32> -> Option<UInt64> -> Option<Int32> -> Option<Int64> -> Option<Single> -> Option<Double> -> Option<Boolean> -> Option<String> -> List<Int32> -> List<Int64> -> List<UInt32> -> List<UInt64> -> List<Int32> -> List<Int64> -> List<UInt32> -> List<UInt64> -> List<Int32> -> List<Int64> -> List<Single> -> List<Double> -> List<Boolean> -> List<String> -> TestAllTypes> 
                ("singleInt64") (fun x -> x.singleInt64) (decode, encode)
        |> fun codec -> 
            let decode =
                let _bind_77adb717e72c4d90898c2d6487648d1c, _ = codec
                _bind_77adb717e72c4d90898c2d6487648d1c
            
            let encode =
                let _, _bind_4711eda9a1264666a0c9707c333d4647 = codec
                _bind_4711eda9a1264666a0c9707c333d4647
            
            Fleece.Newtonsoft.jfieldOpt<TestAllTypes, UInt32, Option<UInt64> -> Option<Int32> -> Option<Int64> -> Option<UInt32> -> Option<UInt64> -> Option<Int32> -> Option<Int64> -> Option<Single> -> Option<Double> -> Option<Boolean> -> Option<String> -> List<Int32> -> List<Int64> -> List<UInt32> -> List<UInt64> -> List<Int32> -> List<Int64> -> List<UInt32> -> List<UInt64> -> List<Int32> -> List<Int64> -> List<Single> -> List<Double> -> List<Boolean> -> List<String> -> TestAllTypes> 
                ("singleUint32") (fun x -> x.singleUint32) (decode, encode)
        |> fun codec -> 
            let decode =
                let _bind_cc06df0cd6474422b555ebb4161d8d3e, _ = codec
                _bind_cc06df0cd6474422b555ebb4161d8d3e
            
            let encode =
                let _, _bind_6fd9577baea14e29928dd293842beb88 = codec
                _bind_6fd9577baea14e29928dd293842beb88
            
            Fleece.Newtonsoft.jfieldOpt<TestAllTypes, UInt64, Option<Int32> -> Option<Int64> -> Option<UInt32> -> Option<UInt64> -> Option<Int32> -> Option<Int64> -> Option<Single> -> Option<Double> -> Option<Boolean> -> Option<String> -> List<Int32> -> List<Int64> -> List<UInt32> -> List<UInt64> -> List<Int32> -> List<Int64> -> List<UInt32> -> List<UInt64> -> List<Int32> -> List<Int64> -> List<Single> -> List<Double> -> List<Boolean> -> List<String> -> TestAllTypes> 
                ("singleUint64") (fun x -> x.singleUint64) (decode, encode)
        |> fun codec -> 
            let decode =
                let _bind_97d44b5066ab4032a88fc38463b818b8, _ = codec
                _bind_97d44b5066ab4032a88fc38463b818b8
            
            let encode =
                let _, _bind_24ffb8720fd64f9294d24adeb2e21280 = codec
                _bind_24ffb8720fd64f9294d24adeb2e21280
            
            Fleece.Newtonsoft.jfieldOpt<TestAllTypes, Int32, Option<Int64> -> Option<UInt32> -> Option<UInt64> -> Option<Int32> -> Option<Int64> -> Option<Single> -> Option<Double> -> Option<Boolean> -> Option<String> -> List<Int32> -> List<Int64> -> List<UInt32> -> List<UInt64> -> List<Int32> -> List<Int64> -> List<UInt32> -> List<UInt64> -> List<Int32> -> List<Int64> -> List<Single> -> List<Double> -> List<Boolean> -> List<String> -> TestAllTypes> 
                ("singleSint32") (fun x -> x.singleSint32) (decode, encode)
        |> fun codec -> 
            let decode =
                let _bind_bf6032a5a06a4b0ba33957a4874b4f3c, _ = codec
                _bind_bf6032a5a06a4b0ba33957a4874b4f3c
            
            let encode =
                let _, _bind_bc6107472d654ece962094a0ca4e0d1f = codec
                _bind_bc6107472d654ece962094a0ca4e0d1f
            
            Fleece.Newtonsoft.jfieldOpt<TestAllTypes, Int64, Option<UInt32> -> Option<UInt64> -> Option<Int32> -> Option<Int64> -> Option<Single> -> Option<Double> -> Option<Boolean> -> Option<String> -> List<Int32> -> List<Int64> -> List<UInt32> -> List<UInt64> -> List<Int32> -> List<Int64> -> List<UInt32> -> List<UInt64> -> List<Int32> -> List<Int64> -> List<Single> -> List<Double> -> List<Boolean> -> List<String> -> TestAllTypes> 
                ("singleSint64") (fun x -> x.singleSint64) (decode, encode)
        |> fun codec -> 
            let decode =
                let _bind_17a4f71c70124184bc96f29f2af025c7, _ = codec
                _bind_17a4f71c70124184bc96f29f2af025c7
            
            let encode =
                let _, _bind_b7e9961a2c8e4bdabde82400ff86554f = codec
                _bind_b7e9961a2c8e4bdabde82400ff86554f
            
            Fleece.Newtonsoft.jfieldOpt<TestAllTypes, UInt32, Option<UInt64> -> Option<Int32> -> Option<Int64> -> Option<Single> -> Option<Double> -> Option<Boolean> -> Option<String> -> List<Int32> -> List<Int64> -> List<UInt32> -> List<UInt64> -> List<Int32> -> List<Int64> -> List<UInt32> -> List<UInt64> -> List<Int32> -> List<Int64> -> List<Single> -> List<Double> -> List<Boolean> -> List<String> -> TestAllTypes> 
                ("singleFixed32") (fun x -> x.singleFixed32) (decode, encode)
        |> fun codec -> 
            let decode =
                let _bind_28a25c05229047dbb5fbbce1675df487, _ = codec
                _bind_28a25c05229047dbb5fbbce1675df487
            
            let encode =
                let _, _bind_bc47c84950ac413cbeb938eeee51cf11 = codec
                _bind_bc47c84950ac413cbeb938eeee51cf11
            
            Fleece.Newtonsoft.jfieldOpt<TestAllTypes, UInt64, Option<Int32> -> Option<Int64> -> Option<Single> -> Option<Double> -> Option<Boolean> -> Option<String> -> List<Int32> -> List<Int64> -> List<UInt32> -> List<UInt64> -> List<Int32> -> List<Int64> -> List<UInt32> -> List<UInt64> -> List<Int32> -> List<Int64> -> List<Single> -> List<Double> -> List<Boolean> -> List<String> -> TestAllTypes> 
                ("singleFixed64") (fun x -> x.singleFixed64) (decode, encode)
        |> fun codec -> 
            let decode =
                let _bind_c6215cf669a4401db1e182d5604f39c0, _ = codec
                _bind_c6215cf669a4401db1e182d5604f39c0
            
            let encode =
                let _, _bind_04191bde715146688eff43a90d5ef3c6 = codec
                _bind_04191bde715146688eff43a90d5ef3c6
            
            Fleece.Newtonsoft.jfieldOpt<TestAllTypes, Int32, Option<Int64> -> Option<Single> -> Option<Double> -> Option<Boolean> -> Option<String> -> List<Int32> -> List<Int64> -> List<UInt32> -> List<UInt64> -> List<Int32> -> List<Int64> -> List<UInt32> -> List<UInt64> -> List<Int32> -> List<Int64> -> List<Single> -> List<Double> -> List<Boolean> -> List<String> -> TestAllTypes> 
                ("singleSfixed32") (fun x -> x.singleSfixed32) (decode, encode)
        |> fun codec -> 
            let decode =
                let _bind_d5e3c5d8fa3845688157e327bb03a765, _ = codec
                _bind_d5e3c5d8fa3845688157e327bb03a765
            
            let encode =
                let _, _bind_d0f54d9f03a3453590b12e665274c7d2 = codec
                _bind_d0f54d9f03a3453590b12e665274c7d2
            
            Fleece.Newtonsoft.jfieldOpt<TestAllTypes, Int64, Option<Single> -> Option<Double> -> Option<Boolean> -> Option<String> -> List<Int32> -> List<Int64> -> List<UInt32> -> List<UInt64> -> List<Int32> -> List<Int64> -> List<UInt32> -> List<UInt64> -> List<Int32> -> List<Int64> -> List<Single> -> List<Double> -> List<Boolean> -> List<String> -> TestAllTypes> 
                ("singleSfixed64") (fun x -> x.singleSfixed64) (decode, encode)
        |> fun codec -> 
            let decode =
                let _bind_28231ec93a6e452db13ed00d11de113a, _ = codec
                _bind_28231ec93a6e452db13ed00d11de113a
            
            let encode =
                let _, _bind_1ab5096f12f646d1872b7d26bcfaa925 = codec
                _bind_1ab5096f12f646d1872b7d26bcfaa925
            
            Fleece.Newtonsoft.jfieldOpt<TestAllTypes, Single, Option<Double> -> Option<Boolean> -> Option<String> -> List<Int32> -> List<Int64> -> List<UInt32> -> List<UInt64> -> List<Int32> -> List<Int64> -> List<UInt32> -> List<UInt64> -> List<Int32> -> List<Int64> -> List<Single> -> List<Double> -> List<Boolean> -> List<String> -> TestAllTypes> 
                ("singleFloat") (fun x -> x.singleFloat) (decode, encode)
        |> fun codec -> 
            let decode =
                let _bind_1c1a765863bd462dbc6c887953828577, _ = codec
                _bind_1c1a765863bd462dbc6c887953828577
            
            let encode =
                let _, _bind_97b855672e524576a3e0c9b3adb53f65 = codec
                _bind_97b855672e524576a3e0c9b3adb53f65
            
            Fleece.Newtonsoft.jfieldOpt<TestAllTypes, Double, Option<Boolean> -> Option<String> -> List<Int32> -> List<Int64> -> List<UInt32> -> List<UInt64> -> List<Int32> -> List<Int64> -> List<UInt32> -> List<UInt64> -> List<Int32> -> List<Int64> -> List<Single> -> List<Double> -> List<Boolean> -> List<String> -> TestAllTypes> 
                ("singleDouble") (fun x -> x.singleDouble) (decode, encode)
        |> fun codec -> 
            let decode =
                let _bind_1123db9793da45ada1ed005524dd14c6, _ = codec
                _bind_1123db9793da45ada1ed005524dd14c6
            
            let encode =
                let _, _bind_3faf6cc943ac4fe58e75275e27efaba8 = codec
                _bind_3faf6cc943ac4fe58e75275e27efaba8
            
            Fleece.Newtonsoft.jfieldOpt<TestAllTypes, Boolean, Option<String> -> List<Int32> -> List<Int64> -> List<UInt32> -> List<UInt64> -> List<Int32> -> List<Int64> -> List<UInt32> -> List<UInt64> -> List<Int32> -> List<Int64> -> List<Single> -> List<Double> -> List<Boolean> -> List<String> -> TestAllTypes> 
                ("singleBool") (fun x -> x.singleBool) (decode, encode)
        |> fun codec -> 
            let decode =
                let _bind_b4a71deaed304156ae192aac83bf6c2f, _ = codec
                _bind_b4a71deaed304156ae192aac83bf6c2f
            
            let encode =
                let _, _bind_af1779d0215d48e3b8c303a6da065b26 = codec
                _bind_af1779d0215d48e3b8c303a6da065b26
            
            Fleece.Newtonsoft.jfieldOpt<TestAllTypes, String, List<Int32> -> List<Int64> -> List<UInt32> -> List<UInt64> -> List<Int32> -> List<Int64> -> List<UInt32> -> List<UInt64> -> List<Int32> -> List<Int64> -> List<Single> -> List<Double> -> List<Boolean> -> List<String> -> TestAllTypes> 
                ("singleString") (fun x -> x.singleString) (decode, encode)
        |> fun codec -> 
            let decode =
                let _bind_9fb8efc736854bfc88182a5ef7aa639e, _ = codec
                _bind_9fb8efc736854bfc88182a5ef7aa639e
            
            let encode =
                let _, _bind_6053c8517d9b47509514063fc5600665 = codec
                _bind_6053c8517d9b47509514063fc5600665
            
            Fleece.Newtonsoft.jfield<TestAllTypes, List<Int32>, List<Int64> -> List<UInt32> -> List<UInt64> -> List<Int32> -> List<Int64> -> List<UInt32> -> List<UInt64> -> List<Int32> -> List<Int64> -> List<Single> -> List<Double> -> List<Boolean> -> List<String> -> TestAllTypes> 
                ("repeatedInt32") (fun x -> x.repeatedInt32) (decode, encode)
        |> fun codec -> 
            let decode =
                let _bind_a1b165c98faa4282a216fb7efa59834b, _ = codec
                _bind_a1b165c98faa4282a216fb7efa59834b
            
            let encode =
                let _, _bind_0f8daa808542437296be5b0d98e9a57e = codec
                _bind_0f8daa808542437296be5b0d98e9a57e
            
            Fleece.Newtonsoft.jfield<TestAllTypes, List<Int64>, List<UInt32> -> List<UInt64> -> List<Int32> -> List<Int64> -> List<UInt32> -> List<UInt64> -> List<Int32> -> List<Int64> -> List<Single> -> List<Double> -> List<Boolean> -> List<String> -> TestAllTypes> 
                ("repeatedInt64") (fun x -> x.repeatedInt64) (decode, encode)
        |> fun codec -> 
            let decode =
                let _bind_2495ea784c5744528477ca97449ba662, _ = codec
                _bind_2495ea784c5744528477ca97449ba662
            
            let encode =
                let _, _bind_4e2d793c66b94078805c93eff04634b8 = codec
                _bind_4e2d793c66b94078805c93eff04634b8
            
            Fleece.Newtonsoft.jfield<TestAllTypes, List<UInt32>, List<UInt64> -> List<Int32> -> List<Int64> -> List<UInt32> -> List<UInt64> -> List<Int32> -> List<Int64> -> List<Single> -> List<Double> -> List<Boolean> -> List<String> -> TestAllTypes> 
                ("repeatedUint32") (fun x -> x.repeatedUint32) (decode, encode)
        |> fun codec -> 
            let decode =
                let _bind_88667d8b1db24f4fbfbdf2d4c7f3fd18, _ = codec
                _bind_88667d8b1db24f4fbfbdf2d4c7f3fd18
            
            let encode =
                let _, _bind_7721a7b1aadc4913b1fe76343bcdea90 = codec
                _bind_7721a7b1aadc4913b1fe76343bcdea90
            
            Fleece.Newtonsoft.jfield<TestAllTypes, List<UInt64>, List<Int32> -> List<Int64> -> List<UInt32> -> List<UInt64> -> List<Int32> -> List<Int64> -> List<Single> -> List<Double> -> List<Boolean> -> List<String> -> TestAllTypes> 
                ("repeatedUint64") (fun x -> x.repeatedUint64) (decode, encode)
        |> fun codec -> 
            let decode =
                let _bind_f0043594c62843aaa82716e4c7e2e6bc, _ = codec
                _bind_f0043594c62843aaa82716e4c7e2e6bc
            
            let encode =
                let _, _bind_493f64f9e09e49db872e31db306433eb = codec
                _bind_493f64f9e09e49db872e31db306433eb
            
            Fleece.Newtonsoft.jfield<TestAllTypes, List<Int32>, List<Int64> -> List<UInt32> -> List<UInt64> -> List<Int32> -> List<Int64> -> List<Single> -> List<Double> -> List<Boolean> -> List<String> -> TestAllTypes> 
                ("repeatedSint32") (fun x -> x.repeatedSint32) (decode, encode)
        |> fun codec -> 
            let decode =
                let _bind_066a3faedcbc41a4b7b0126a294c7a70, _ = codec
                _bind_066a3faedcbc41a4b7b0126a294c7a70
            
            let encode =
                let _, _bind_3d126fdf90db4f84a0e05a2e6fc90173 = codec
                _bind_3d126fdf90db4f84a0e05a2e6fc90173
            
            Fleece.Newtonsoft.jfield<TestAllTypes, List<Int64>, List<UInt32> -> List<UInt64> -> List<Int32> -> List<Int64> -> List<Single> -> List<Double> -> List<Boolean> -> List<String> -> TestAllTypes> 
                ("repeatedSint64") (fun x -> x.repeatedSint64) (decode, encode)
        |> fun codec -> 
            let decode =
                let _bind_f16352202bc241f5b43e6fbc43dd5fea, _ = codec
                _bind_f16352202bc241f5b43e6fbc43dd5fea
            
            let encode =
                let _, _bind_bee83f8ec2ce4c41af94f48d4671f516 = codec
                _bind_bee83f8ec2ce4c41af94f48d4671f516
            
            Fleece.Newtonsoft.jfield<TestAllTypes, List<UInt32>, List<UInt64> -> List<Int32> -> List<Int64> -> List<Single> -> List<Double> -> List<Boolean> -> List<String> -> TestAllTypes> 
                ("repeatedFixed32") (fun x -> x.repeatedFixed32) (decode, encode)
        |> fun codec -> 
            let decode =
                let _bind_496b757fb8b545f3a063ae06eaed3f18, _ = codec
                _bind_496b757fb8b545f3a063ae06eaed3f18
            
            let encode =
                let _, _bind_64eb7cece04f4f84989503e56cd31516 = codec
                _bind_64eb7cece04f4f84989503e56cd31516
            
            Fleece.Newtonsoft.jfield<TestAllTypes, List<UInt64>, List<Int32> -> List<Int64> -> List<Single> -> List<Double> -> List<Boolean> -> List<String> -> TestAllTypes> 
                ("repeatedFixed64") (fun x -> x.repeatedFixed64) (decode, encode)
        |> fun codec -> 
            let decode =
                let _bind_b7690646d6a541e38644a732e2233cd2, _ = codec
                _bind_b7690646d6a541e38644a732e2233cd2
            
            let encode =
                let _, _bind_f54dc2827c6a4d4ab2fd1aa3cd9dd4db = codec
                _bind_f54dc2827c6a4d4ab2fd1aa3cd9dd4db
            
            Fleece.Newtonsoft.jfield<TestAllTypes, List<Int32>, List<Int64> -> List<Single> -> List<Double> -> List<Boolean> -> List<String> -> TestAllTypes> 
                ("repeatedSfixed32") (fun x -> x.repeatedSfixed32) (decode, encode)
        |> fun codec -> 
            let decode =
                let _bind_061df61c2861426587b6f422fb4be5af, _ = codec
                _bind_061df61c2861426587b6f422fb4be5af
            
            let encode =
                let _, _bind_c96c9e9e8253413a9c237e51396ca7e5 = codec
                _bind_c96c9e9e8253413a9c237e51396ca7e5
            
            Fleece.Newtonsoft.jfield<TestAllTypes, List<Int64>, List<Single> -> List<Double> -> List<Boolean> -> List<String> -> TestAllTypes> 
                ("repeatedSfixed64") (fun x -> x.repeatedSfixed64) (decode, encode)
        |> fun codec -> 
            let decode =
                let _bind_6db1c0e6e4ac4d1d8d4d39b3debde3df, _ = codec
                _bind_6db1c0e6e4ac4d1d8d4d39b3debde3df
            
            let encode =
                let _, _bind_aebba320903f4fdfa51c45c34142c18f = codec
                _bind_aebba320903f4fdfa51c45c34142c18f
            
            Fleece.Newtonsoft.jfield<TestAllTypes, List<Single>, List<Double> -> List<Boolean> -> List<String> -> TestAllTypes> 
                ("repeatedFloat") (fun x -> x.repeatedFloat) (decode, encode)
        |> fun codec -> 
            let decode =
                let _bind_8bc245d4c61b4228be032e800a770277, _ = codec
                _bind_8bc245d4c61b4228be032e800a770277
            
            let encode =
                let _, _bind_74ed1ccd1f394f788c027440fb1d8a05 = codec
                _bind_74ed1ccd1f394f788c027440fb1d8a05
            
            Fleece.Newtonsoft.jfield<TestAllTypes, List<Double>, List<Boolean> -> List<String> -> TestAllTypes> 
                ("repeatedDouble") (fun x -> x.repeatedDouble) (decode, encode)
        |> fun codec -> 
            let decode =
                let _bind_e858c2f636ec40eba3c1f45f8f87d15e, _ = codec
                _bind_e858c2f636ec40eba3c1f45f8f87d15e
            
            let encode =
                let _, _bind_bcfed51e29a04a7f8f36f18477e64f0e = codec
                _bind_bcfed51e29a04a7f8f36f18477e64f0e
            
            Fleece.Newtonsoft.jfield<TestAllTypes, List<Boolean>, List<String> -> TestAllTypes> ("repeatedBool") 
                (fun x -> x.repeatedBool) (decode, encode)
        |> fun codec -> 
            let decode =
                let _bind_09daf2017c9d4ff696edc3d6cd8a15de, _ = codec
                _bind_09daf2017c9d4ff696edc3d6cd8a15de
            
            let encode =
                let _, _bind_671d688558ae40c9b6f197f223c1eab2 = codec
                _bind_671d688558ae40c9b6f197f223c1eab2
            
            Fleece.Newtonsoft.jfield<TestAllTypes, List<String>, TestAllTypes> ("repeatedString") 
                (fun x -> x.repeatedString) (decode, encode)
    
    static member Serialize(m : TestAllTypes, buffer : ZeroCopyBuffer) =
        Primitives.writeOption<Int32> (Primitives.writeInt32) (1) (buffer) (m.singleInt32)
        Primitives.writeOption<Int64> (Primitives.writeInt64) (2) (buffer) (m.singleInt64)
        Primitives.writeOption<UInt32> (Primitives.writeUInt32) (3) (buffer) (m.singleUint32)
        Primitives.writeOption<UInt64> (Primitives.writeUInt64) (4) (buffer) (m.singleUint64)
        Primitives.writeOption<Int32> (Primitives.writeSInt32) (5) (buffer) (m.singleSint32)
        Primitives.writeOption<Int64> (Primitives.writeSInt64) (6) (buffer) (m.singleSint64)
        Primitives.writeOption<UInt32> (Primitives.writeFixed32) (7) (buffer) (m.singleFixed32)
        Primitives.writeOption<UInt64> (Primitives.writeFixed64) (8) (buffer) (m.singleFixed64)
        Primitives.writeOption<Int32> (Primitives.writeSFixed32) (9) (buffer) (m.singleSfixed32)
        Primitives.writeOption<Int64> (Primitives.writeSFixed64) (10) (buffer) (m.singleSfixed64)
        Primitives.writeOption<Single> (Primitives.writeFloat) (11) (buffer) (m.singleFloat)
        Primitives.writeOption<Double> (Primitives.writeDouble) (12) (buffer) (m.singleDouble)
        Primitives.writeOption<Boolean> (Primitives.writeBool) (13) (buffer) (m.singleBool)
        Primitives.writeOption<String> (Primitives.writeString) (14) (buffer) (m.singleString)
        Primitives.writeRepeated<Int32> (Primitives.writeInt32) (31) (buffer) (m.repeatedInt32)
        Primitives.writeRepeated<Int64> (Primitives.writeInt64) (32) (buffer) (m.repeatedInt64)
        Primitives.writeRepeated<UInt32> (Primitives.writeUInt32) (33) (buffer) (m.repeatedUint32)
        Primitives.writeRepeated<UInt64> (Primitives.writeUInt64) (34) (buffer) (m.repeatedUint64)
        Primitives.writeRepeated<Int32> (Primitives.writeSInt32) (35) (buffer) (m.repeatedSint32)
        Primitives.writeRepeated<Int64> (Primitives.writeSInt64) (36) (buffer) (m.repeatedSint64)
        Primitives.writeRepeated<UInt32> (Primitives.writeFixed32) (37) (buffer) (m.repeatedFixed32)
        Primitives.writeRepeated<UInt64> (Primitives.writeFixed64) (38) (buffer) (m.repeatedFixed64)
        Primitives.writeRepeated<Int32> (Primitives.writeSFixed32) (39) (buffer) (m.repeatedSfixed32)
        Primitives.writeRepeated<Int64> (Primitives.writeSFixed64) (40) (buffer) (m.repeatedSfixed64)
        Primitives.writeRepeated<Single> (Primitives.writeFloat) (41) (buffer) (m.repeatedFloat)
        Primitives.writeRepeated<Double> (Primitives.writeDouble) (42) (buffer) (m.repeatedDouble)
        Primitives.writeRepeated<Boolean> (Primitives.writeBool) (43) (buffer) (m.repeatedBool)
        Primitives.writeRepeated<String> (Primitives.writeString) (44) (buffer) (m.repeatedString)
    
    static member Deserialize(buffer : ZeroCopyBuffer) = Primitives.deserialize<TestAllTypes> (buffer)
    interface IMessage with
        member x.Serialize(buffer : ZeroCopyBuffer) = TestAllTypes.Serialize(x, buffer)
        
        member x.ReadFrom(buffer : ZeroCopyBuffer) =
            let enumerator : IEnumerator<Froto.Serialization.Encoding.RawField> =
                ZeroCopyBuffer.allFields(buffer).GetEnumerator()
            while enumerator.MoveNext() do
                let current : Froto.Serialization.Encoding.RawField = enumerator.Current
                if current.FieldNum = 44 then 
                    if Operators.isNull<List<String>> (x.repeatedString) then x.repeatedString <- new List<String>()
                    else ()
                    x.repeatedString.Add(Primitives.readString current)
                else if current.FieldNum = 43 then 
                    if Operators.isNull<List<Boolean>> (x.repeatedBool) then x.repeatedBool <- new List<Boolean>()
                    else ()
                    x.repeatedBool.Add(Primitives.readBool current)
                else if current.FieldNum = 42 then 
                    if Operators.isNull<List<Double>> (x.repeatedDouble) then x.repeatedDouble <- new List<Double>()
                    else ()
                    x.repeatedDouble.Add(Primitives.readDouble current)
                else if current.FieldNum = 41 then 
                    if Operators.isNull<List<Single>> (x.repeatedFloat) then x.repeatedFloat <- new List<Single>()
                    else ()
                    x.repeatedFloat.Add(Primitives.readFloat current)
                else if current.FieldNum = 40 then 
                    if Operators.isNull<List<Int64>> (x.repeatedSfixed64) then x.repeatedSfixed64 <- new List<Int64>()
                    else ()
                    x.repeatedSfixed64.Add(Primitives.readSFixed64 current)
                else if current.FieldNum = 39 then 
                    if Operators.isNull<List<Int32>> (x.repeatedSfixed32) then x.repeatedSfixed32 <- new List<Int32>()
                    else ()
                    x.repeatedSfixed32.Add(Primitives.readSFixed32 current)
                else if current.FieldNum = 38 then 
                    if Operators.isNull<List<UInt64>> (x.repeatedFixed64) then x.repeatedFixed64 <- new List<UInt64>()
                    else ()
                    x.repeatedFixed64.Add(Primitives.readFixed64 current)
                else if current.FieldNum = 37 then 
                    if Operators.isNull<List<UInt32>> (x.repeatedFixed32) then x.repeatedFixed32 <- new List<UInt32>()
                    else ()
                    x.repeatedFixed32.Add(Primitives.readFixed32 current)
                else if current.FieldNum = 36 then 
                    if Operators.isNull<List<Int64>> (x.repeatedSint64) then x.repeatedSint64 <- new List<Int64>()
                    else ()
                    x.repeatedSint64.Add(Primitives.readSInt64 current)
                else if current.FieldNum = 35 then 
                    if Operators.isNull<List<Int32>> (x.repeatedSint32) then x.repeatedSint32 <- new List<Int32>()
                    else ()
                    x.repeatedSint32.Add(Primitives.readSInt32 current)
                else if current.FieldNum = 34 then 
                    if Operators.isNull<List<UInt64>> (x.repeatedUint64) then x.repeatedUint64 <- new List<UInt64>()
                    else ()
                    x.repeatedUint64.Add(Primitives.readUInt64 current)
                else if current.FieldNum = 33 then 
                    if Operators.isNull<List<UInt32>> (x.repeatedUint32) then x.repeatedUint32 <- new List<UInt32>()
                    else ()
                    x.repeatedUint32.Add(Primitives.readUInt32 current)
                else if current.FieldNum = 32 then 
                    if Operators.isNull<List<Int64>> (x.repeatedInt64) then x.repeatedInt64 <- new List<Int64>()
                    else ()
                    x.repeatedInt64.Add(Primitives.readInt64 current)
                else if current.FieldNum = 31 then 
                    if Operators.isNull<List<Int32>> (x.repeatedInt32) then x.repeatedInt32 <- new List<Int32>()
                    else ()
                    x.repeatedInt32.Add(Primitives.readInt32 current)
                else if current.FieldNum = 14 then 
                    x.singleString <- (Some(Primitives.readString current) : Option<String>)
                else if current.FieldNum = 13 then x.singleBool <- (Some(Primitives.readBool current) : Option<Boolean>)
                else if current.FieldNum = 12 then 
                    x.singleDouble <- (Some(Primitives.readDouble current) : Option<Double>)
                else if current.FieldNum = 11 then 
                    x.singleFloat <- (Some(Primitives.readFloat current) : Option<Single>)
                else if current.FieldNum = 10 then 
                    x.singleSfixed64 <- (Some(Primitives.readSFixed64 current) : Option<Int64>)
                else if current.FieldNum = 9 then 
                    x.singleSfixed32 <- (Some(Primitives.readSFixed32 current) : Option<Int32>)
                else if current.FieldNum = 8 then 
                    x.singleFixed64 <- (Some(Primitives.readFixed64 current) : Option<UInt64>)
                else if current.FieldNum = 7 then 
                    x.singleFixed32 <- (Some(Primitives.readFixed32 current) : Option<UInt32>)
                else if current.FieldNum = 6 then 
                    x.singleSint64 <- (Some(Primitives.readSInt64 current) : Option<Int64>)
                else if current.FieldNum = 5 then 
                    x.singleSint32 <- (Some(Primitives.readSInt32 current) : Option<Int32>)
                else if current.FieldNum = 4 then 
                    x.singleUint64 <- (Some(Primitives.readUInt64 current) : Option<UInt64>)
                else if current.FieldNum = 3 then 
                    x.singleUint32 <- (Some(Primitives.readUInt32 current) : Option<UInt32>)
                else if current.FieldNum = 2 then x.singleInt64 <- (Some(Primitives.readInt64 current) : Option<Int64>)
                else if current.FieldNum = 1 then x.singleInt32 <- (Some(Primitives.readInt32 current) : Option<Int32>)
                else ()
            enumerator.Dispose()
        
        member x.SerializedLength() = Primitives.serializedLength<TestAllTypes> (x)
