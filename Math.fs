namespace Trading.Studies

module Math =

  open Trading.Studies

  let sumLookback period = period - 1

  [<TradingStudy;
    Group("Math");
    Title("Summation");
    Description("Returns the sum of the last n observations")
  >]
  let sum period (data:float[]) =
    Study.checkPositiveIntMin1 period
    let lookbackTotal = sumLookback period
    let startIdx = lookbackTotal
    let endIdx = data.Length - 1
    Study.lazyCompute startIdx endIdx (fun outLen ->
      let out = Array.zeroCreate outLen

      for i in startIdx - lookbackTotal .. startIdx do
        out.[0] <- out.[0] + data.[i]

      for today in startIdx + 1 .. endIdx do
        let outIdx = today - startIdx
        out.[outIdx] <- out.[outIdx - 1] + data.[today] - data.[today - period]

      out
    )

  //=====================================================

  let prodLookback period = period - 1

  let rec productAux today stop acc (data:float[]) =
    if acc = 0.0 then 0.0
    elif today <= stop then
      productAux (today+1) stop (data.[today] *acc) data
    else acc

  [<TradingStudy;
    Group("Math");
    Title("Product");
    Description("Returns the product of the last n observations")
  >]
  let prod period (data:float[]) =
    Study.checkPositiveIntMin1 period
    let lookbackTotal = prodLookback period
    let startIdx = lookbackTotal
    let endIdx = data.Length - 1
    Study.lazyCompute startIdx endIdx (fun outLen ->
      let out = Array.zeroCreate outLen

      out.[0] <- data |> productAux (startIdx - lookbackTotal) startIdx 1.0

      for today in startIdx + 1 .. endIdx do
        let outIdx = today - startIdx
        let outNew =
          match data.[today - period], out.[outIdx - 1] with
          | 0.0, _ -> data |> productAux (today - lookbackTotal) today 1.0
          | _, 0.0 -> 0.0
          | dataPrev,  outPrev -> outPrev / dataPrev * data.[today]
        out.[outIdx] <- outNew

      out
    )

  //=====================================================

  let scaleLookback = 0

  [<TradingStudy;
    Group("Math");
    Title("Scale");
    Description("Returns the observations multiplied by a coefficient")
  >]
  let scale weight (data:float[]) =
    let lookbackTotal = scaleLookback
    let startIdx = lookbackTotal
    let endIdx = data.Length - 1
    Study.lazyCompute startIdx endIdx (fun outLen ->
      let f =
        match weight with
        | 0.0 -> fun idx -> 0.0
        | 1.0 -> fun idx -> data.[idx]
        | w -> fun idx -> w * data.[idx]

      Array.init outLen f
    )

  //=====================================================

  let shiftLookback = 0

  [<TradingStudy;
    Group("Math");
    Title("Scale");
    Description("Returns the observations shifted by a coefficient")
  >]
  let shift delta (data:float[]) =
    let lookbackTotal = shiftLookback
    let startIdx = lookbackTotal
    let endIdx = data.Length - 1
    Study.lazyCompute startIdx endIdx (fun outLen ->
      let f =
        match delta with
        | 0.0 -> fun idx -> data.[idx]
        | x -> fun idx -> x + data.[idx]
      Array.init outLen f
    )

  //=====================================================

  let waddLookback = 0

  [<TradingStudy;
    Group("Math");
    Title("Weighted addition");
    Description("Returns the weighted sum of two series");
    MultipleInputSeriesAttribute;
  >]
  let wadd weight1 weight2 (data1:float[]) data2 =
    Study.checkSameInputLength [data1; data2]
    let lookbackTotal = waddLookback
    let startIdx = lookbackTotal
    let endIdx = data1.Length - 1
    Study.lazyCompute startIdx endIdx (fun outLen ->
      let f =
        match weight1, weight2 with
        | 0.0, 0.0 -> fun idx -> 0.0
        | 0.0, 1.0 -> fun idx -> data2.[idx]
        | 0.0, beta -> fun idx -> beta * data2.[idx]
        | 1.0, 0.0 -> fun idx -> data1.[idx]
        | alpha, 0.0 -> fun idx -> alpha * data1.[idx]
        | 1.0, 1.0 -> fun idx -> data1.[idx] + data2.[idx]
        | alpha, beta -> fun idx -> alpha * data1.[idx] + beta * data2.[idx]

      Array.init outLen (fun i -> f (startIdx + i))
    )

  //=====================================================

  let addLookback = waddLookback

  [<TradingStudy;
    Group("Math");
    Title("Addition");
    Description("Returns the sum of two series");
    MultipleInputSeriesAttribute;
  >]
  let add data1 data2 =
    wadd 1.0 1.0 data1 data2

  //=====================================================

  let subLookback = waddLookback

  [<TradingStudy;
    Group("Math");
    Title("Substraction");
    Description("Returns the difference between two series");
    MultipleInputSeriesAttribute;
  >]
  let sub data1 data2 =
    wadd 1.0 (-1.0) data1 data2

  //=====================================================

  let multLookback = 0

  [<TradingStudy;
    Group("Math");
    Title("Multiplication");
    Description("Returns the product of two series");
    MultipleInputSeriesAttribute;
  >]
  let mult (data1:float[]) data2 =
    Study.checkSameInputLength [data1; data2]
    let lookbackTotal = waddLookback
    let startIdx = lookbackTotal
    let endIdx = data1.Length - 1
    Study.lazyCompute startIdx endIdx (fun outLen ->
      Array.init outLen (fun i ->
        let today = startIdx + i
        data1.[today] * data2.[today]
      )
    )

  //=====================================================

  let divLookback = 0

  [<TradingStudy;
    Group("Math");
    Title("Division");
    Description("Returns the ratio of two series");
    MultipleInputSeriesAttribute;
  >]
  let div valueIfDenominatorIsZero (data1:float[]) data2 =
    Study.checkSameInputLength [data1; data2]
    let lookbackTotal = waddLookback
    let startIdx = lookbackTotal
    let endIdx = data1.Length - 1
    Study.lazyCompute startIdx endIdx (fun outLen ->
      Array.init outLen (fun i ->
        let today = startIdx + i
        if data2.[today] <> 0.0 then data1.[today] / data2.[today] else valueIfDenominatorIsZero
      )
    )

