//Esample of use
//do for KeyValue(name, info) in Trading.Chart.Reflection.getStudiesInfo()  do
//      printfn "%s : %A" name info
namespace Trading.Chart

open System
open System.Reflection
open Microsoft.FSharp.Reflection

open Trading.Studies

//
//  Store info
//

module Reflection =

  type ParamInfo(p:ParameterInfo) =

    let name =
      let arr = p.GetCustomAttributes(typeof<DisplayNameAttribute>, false)
      if arr.Length = 0 then p.Name else (arr.[0] :?> DisplayNameAttribute).Name

    let defaultValue =
      let arr = p.GetCustomAttributes(typeof<DefaultValueAttribute>, false)
      if arr.Length = 0 then None else Some (arr.[0] :?> DefaultValueAttribute)

    let isBool =
      let arr = p.GetCustomAttributes(typeof<BoolAttribute>, false)
      arr.Length <> 0

    let numAttr =
      let arr = p.GetCustomAttributes(typeof<NumericAttribute>, false)
      if arr.Length = 0 then None else Some (arr.[0] :?> NumericAttribute)

    let allPositiveAttr =
      let arr = p.GetCustomAttributes(typeof<AllPositiveAttribute>, false)
      arr.Length <> 0

    member x.Name = name
    member x.Type = p.ParameterType.Name
    member x.Position = p.Position
    member x.IsBool = isBool
    member x.NumericAttribute = numAttr
    member x.AllPositiveAttribute = allPositiveAttr
    override x.ToString() =
      sprintf "{Name=%s; Type=%s; Position:%d; IsBool:%A; IsNumeric:%s}"
        x.Name x.Type x.Position x.IsBool
        ( match x.NumericAttribute with
          | None ->
              match x.Type.ToLower() with
              | "int32"
              | "double" -> "yes;"
              | _ -> "no;"
          | Some attr ->
              let min = if attr.MinValue = "nan" then "" else sprintf "min:%s; " attr.MinValue
              let max = if attr.MaxValue = "nan" then "" else sprintf "max:%s; " attr.MaxValue
              let step = if attr.Step = "nan" then "" else sprintf "step:%s;" attr.Step
              let np =
                if (min.Length > 0 || max.Length > 0 || step.Length > 0) then
                  sprintf " NumericParams:{%s%s%s}" min max step
                else ""
              sprintf "yes;%s" np
        )

  type StudyInfo = {
    Group : string
    Title : string
    Description : string
    DoesOverlay : bool
    HasMultipleInputSeries : bool
    Parameters : ParamInfo[]
    OutputTypes : string[]
    OutputSeriesNames : string[]
  } with
      static member create g t desc d multipleSeries p ot series = {
        Group = g
        Title = t
        Description = desc
        DoesOverlay = d
        HasMultipleInputSeries = multipleSeries
        Parameters = p
        OutputTypes = ot
        OutputSeriesNames = series
      }

//====================================================================
//
//  Extract the meta information from studies within the
//  assembly containing "Trading.Studies.TradingStudyAttribute"
//
//====================================================================

  let getMetainfo (m:MethodInfo) =
    let ps =
      m.GetParameters() |> Array.map (fun p -> new ParamInfo(p))

    let outTypes =
      let ty = m.ReturnType
      if FSharpType.IsTuple ty then
        FSharpType.GetTupleElements ty |> Array.map (fun ty -> ty.Name)
      else
        [|ty.Name|]

    let group =
      let arr = m.GetCustomAttributes((typeof<GroupAttribute>),false)
      if arr.Length = 0 then ""
      else (arr.[0] :?> GroupAttribute).Group

    let title =
      let arr = m.GetCustomAttributes((typeof<TitleAttribute>),false)
      if arr.Length = 0 then ""
      else (arr.[0] :?> TitleAttribute).Title

    let description =
      let arr = m.GetCustomAttributes((typeof<DescriptionAttribute>),false)
      if arr.Length = 0 then ""
      else (arr.[0] :?> DescriptionAttribute).Description

    let hasMultipleSeries =
      let arr = m.GetCustomAttributes((typeof<MultipleInputSeriesAttribute>),false)
      arr.Length <> 0

    let seriesNames =
      let arr = m.GetCustomAttributes((typeof<OutputSeriesNamesAttribute>),false)
      if arr.Length = 0 then [|"output"|]
      else (arr.[0] :?> OutputSeriesNamesAttribute).Series

    let doesOverlay =
      let arr = m.GetCustomAttributes((typeof<OverlayAttribute>),false)
      arr.Length <> 0

    StudyInfo.create group title description doesOverlay hasMultipleSeries ps outTypes seriesNames

  let isStudy (m:MemberInfo) =
    (m.GetCustomAttributes((typeof<TradingStudyAttribute>),false)).Length <> 0
    && (m :? MethodInfo)

  let getStudiesInfo() =
    typeof<Trading.Studies.TradingStudyAttribute>.Assembly.GetTypes()
      |> Array.filter FSharpType.IsModule
      |> Array.map (fun m ->
            m.GetMembers()
            |> Array.filter isStudy
          )
      |> Array.concat
      |> Array.map (fun m -> (m.Name, getMetainfo (m :?> MethodInfo)))
      |> Map.ofArray
