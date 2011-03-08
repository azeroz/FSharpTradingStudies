namespace Trading.Data

open System

type PriceSeries =
    { Date : DateTime[]
      Open : float[]
      High : float[]
      Low : float[]
      Close : float[]
      Volume : float[]
    }
      static member create n =
        { Date = Array.zeroCreate n
          Open = Array.zeroCreate n
          High = Array.zeroCreate n
          Low = Array.zeroCreate n
          Close = Array.zeroCreate n
          Volume = Array.zeroCreate n
        }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module PriceSeries =

  let create n =
    { Date = Array.zeroCreate n
      Open = Array.zeroCreate n
      High = Array.zeroCreate n
      Low = Array.zeroCreate n
      Close = Array.zeroCreate n
      Volume = Array.zeroCreate n
    }

  let rec private areDatesInvalidAux dates i found =
    if found then true
    elif i >= Array.length dates then found
    else
      areDatesInvalidAux dates (i+1) (dates.[i] < dates.[i-1])

  let checkDates dates =
    if Array.length dates > 1 && areDatesInvalidAux dates 1 false then
      invalidArg "dates" "dates are not sorted"

  let checkSameLength a b =
    if Array.length a <> Array.length b then
      invalidArg "input arrays" "input arrays have different lengths"

  let checkHighLow high low =
    checkSameLength high low
    for i in 0 .. high.Length - 1 do
      if high.[i] < low.[i] then
        failwithf "high < low at index %d" i

  let checkHighLowClose high low c =
    checkSameLength high low
    checkSameLength high c
    for i in 0 .. Array.length high - 1 do
      if c.[i] > high.[i] then
        failwithf "open > high at index %d" i
      if low.[i] > c.[i] then
        failwithf "low > open at index %d" i

  let checkOpenHighLowClose o high low c =
    checkSameLength high low
    checkSameLength high o
    checkSameLength high c
    for i in 0 .. Array.length high - 1 do
      if o.[i] > high.[i] then
        failwithf "open > high at index %d" i
      if low.[i] > o.[i] then
        failwithf "low > open at index %d" i
      if c.[i] > high.[i] then
        failwithf "close > high at index %d" i
      if low.[i] > c.[i] then
        failwithf "low > close at index %d" i

