namespace Trading.Studies

exception BadParam of string

open System
open System.Reflection
open Microsoft.FSharp.Reflection

//
//  General flags
//

///Indicates the function shall be marked as a study.
type TradingStudyAttribute() =
  inherit System.Attribute()

///Group of studies the study belongs to.
type GroupAttribute(group:string) =
  inherit System.Attribute()
  member self.Group = group

///Title of the study.
type TitleAttribute(title:string) =
  inherit System.Attribute()
  member self.Title = title

///Description of the study.
type DescriptionAttribute(desc:string) =
  inherit System.Attribute()
  member self.Description = desc

///Indicates that for studies with different input series, the said series need not belong
///to the same securty. For instance, if you compute the covariance, you may need
///the close of two stocks, while if you compute the daily high/low range, the input series
///shall belong to the same security.
type MultipleInputSeriesAttribute() =
  inherit System.Attribute()

//
//  Output flags
//

///Indicates that the study should be displayed on the same chart as
///the input series.
type OverlayAttribute() =
  inherit System.Attribute()

///When the study outputs several several series, this attribute
///allows to indicate their name by seperating them with a comma
///(e.g. [<OutputSeriesNames("a,b,c")>]).
type OutputSeriesNamesAttribute(series:string) =
  inherit System.Attribute()
  let series =
    series.Split([|','|], StringSplitOptions.RemoveEmptyEntries)
    |> Array.map (fun s -> s.Trim())
  member self.Series = series

//
//  Input Parameters flags
//

//To be put into practice in the various functions

///Indicate the name of the parameter.
type DisplayNameAttribute(name:string) =
  inherit System.Attribute()
  member self.Name = name

///Indicates the parameter is a bool.
type BoolAttribute() =
  inherit System.Attribute()

///Indicates that all values of the input series shall be positive.
type AllPositiveAttribute() =
  inherit System.Attribute()

///Indicates the default value of the parameter as a string.
///Obviously, you must know the parameter type to use this piece
///of information.
type DefaultValueAttribute(v:string) =
  inherit System.Attribute()
  member x.Value = v

///Indicates the minimum, maximum and step of a numeric parameter.
///Values are strings so that it can be used for any parameter type.
///Obviously, you must know the latter to use this piece
///of information.
type NumericAttribute(minV:string, maxV:string, step:string) =
  inherit System.Attribute()
  member x.MinValue = minV
  member x.MaxValue = maxV
  member x.Step = step     

module internal Study =

  let checkPositiveReal v =
    if v < 0.0 then raise <| BadParam "negative real"

  let checkPositiveInt v =
    if v < 0 then raise <| BadParam "negative int"

  let checkPositiveIntMin1 v =
    if v < 1 then raise <| BadParam "int less than one"

  let checkVolume v =
    if Array.exists (fun x -> x < 0.0) v then raise <| BadParam "negative volume"

  let checkSameInputLength xs =
    let len = Seq.head xs |> Seq.length
    if Seq.exists (fun x -> Seq.length x <> len) xs then
       raise <| BadParam "input series have different length"

  let checkLength x n =
    if Seq.length x <> n then
      raise <| BadParam "unexpected input length"

  let rec checkHighLow hs ls =
    checkSameInputLength [hs; ls]
    checkHL hs ls 0 true

  and checkHL hs ls i ok =
    if not ok then
      let msg = sprintf "low > high at %d" i
      raise <| BadParam msg
    elif i < Array.length hs then
      if hs.[i] >= ls.[i] then
        checkHL hs ls (i+1) true
      else
        checkHL hs ls i false

  let rec checkHighLowClose hs ls cs =
    checkSameInputLength [hs; ls; cs]
    checkHLC hs ls cs 0 true

  and checkHLC hs ls cs i ok =
    if not ok then
      let msg = sprintf "low > high at %d" i
      raise <| BadParam msg
    elif i < Array.length hs then
      if hs.[i] >= cs.[i] && cs.[i] >= ls.[i] then
        checkHLC hs ls cs (i+1) true
      else
        checkHLC hs ls cs i false

  let rec checkOpenHighLowClose os hs ls cs =
    checkSameInputLength [os; hs; ls; cs]
    checkOHLC os hs ls cs 0 true

  and checkOHLC os hs ls cs i ok =
    if not ok then
      let msg = sprintf "low > high at %d" i
      raise <| BadParam msg
    elif i < Array.length hs then
      if isOkOHLC os hs ls cs i then
        checkOHLC os hs ls cs (i+1) true
      else
        checkOHLC os hs ls cs i false

  and isOkOHLC os hs ls cs i =
      if cs.[i] > os.[i] then
        hs.[i] >= cs.[i] && os.[i] >= ls.[i]
      else
        hs.[i] >= os.[i] && cs.[i] >= ls.[i]

  let lazyCompute (startIdx:int) endIdx f =
    if startIdx > endIdx then [||] else f (endIdx - startIdx + 1)

  let lazyCompute2 (startIdx:int) endIdx f =
    if startIdx > endIdx then [||], [||] else f (endIdx - startIdx + 1)

  let lazyCompute3 (startIdx:int) endIdx f =
    if startIdx > endIdx then [||],[||],[||] else f (endIdx - startIdx + 1)

  let lazyCompute4 (startIdx:int) endIdx f =
    if startIdx > endIdx then [||],[||],[||],[||] else f (endIdx - startIdx + 1)

  let median (x:float[]) =
    let a = Array.sort x
    let n = Array.length a
    let middle = n / 2
    if n = 2 * middle then
      0.5 * (a.[middle] + a.[middle - 1])
    else
      a.[middle] 

  let circularIndex idx data = idx % Array.length data 

  let degToRadConversionFactor = 180.0 / System.Math.PI

  let degToRad x = x * degToRadConversionFactor

  let radToDeg x = x / degToRadConversionFactor

  let slopeToAngle isRadian slope =
    if isRadian then atan slope else (atan slope) / System.Math.PI * 180.0

  let safeWAdd (data:float[]) (data2:float[]) lookback a b =
    if data.Length = data2.Length then
      let f =
        match a, b with
        | 0.0, 0.0 -> fun x y -> 0.0
        | 0.0, 1.0 -> fun x y -> y
        | 0.0, _ -> fun x y -> b * y
        | 1.0, 0.0 -> fun x y -> x
        | _, 0.0 -> fun x y -> a * x
        | 1.0, 1.0 -> fun x y -> x + y
        | _, _ -> fun x y -> a*x + b*y
      Array.map2 f data data2
    else
      let offset = data.Length - data2.Length
      if offset > 0 then
        let f =
          match a, b with
          | 0.0, 0.0 -> fun i y -> 0.0
          | 0.0, 1.0 -> fun i y -> y
          | 0.0, _ -> fun i y -> b * y
          | 1.0, 0.0 -> fun i y -> data.[i+lookback]
          | _, 0.0 -> fun i y -> a*data.[i+lookback]
          | 1.0, 1.0 -> fun i y -> data.[i+lookback] + y
          | _, _ -> fun i y -> a*data.[i+lookback] + b*y
        Array.mapi f data2
      else
        let f =
          match a, b with
          | 0.0, 0.0 -> fun i x -> 0.0
          | 0.0, 1.0 -> fun i x -> x
          | 0.0, _ -> fun i x -> a * x
          | 1.0, 0.0 -> fun i x -> data2.[i-lookback]
          | _, 0.0 -> fun i x -> b*data2.[i-lookback]
          | 1.0, 1.0 -> fun i x -> x + data2.[i-lookback]
          | _, _ -> fun i x -> a*x + b*data2.[i-lookback]
        Array.mapi f data

  let extremas data =
    data |> Array.fold (fun (curMin, curMax) x ->
      if x < curMin then (x, curMax)
      elif x > curMax then (curMin, x)
      else (curMin, curMax)
    ) (data.[0], data.[0])

  let toRange newMin newMAx data =
    let newRange = newMAx - newMin
    let oldMin, oldMAx = extremas data
    let oldRange = oldMAx - oldMin
    if oldRange <> 0.0 then
      let convert x =
        let normalizedX = (x - oldMin) / oldRange
        newMin + (normalizedX * newRange)
      Array.map convert data
    else
      Array.create (Array.length data) (newMin + (newRange * 0.5))



