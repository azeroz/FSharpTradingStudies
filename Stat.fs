namespace Trading.Studies

module Stat =

  open Trading.Studies

  let cmpLookback period = period - 1

  let rec movingCompareAux today endIdx extremaPosition cmpF (data:float[]) =
    if today > endIdx then extremaPosition
    else
      let newPosition =
        if cmpF data.[today] data.[extremaPosition] then
          today
        else
          extremaPosition
      movingCompareAux (today+1) endIdx newPosition cmpF data

  let cmp cmpF period (data:float[]) =
    Study.checkPositiveIntMin1 period
    let lookbackTotal = cmpLookback period
    let startIdx = lookbackTotal
    let endIdx = data.Length - 1
    Study.lazyCompute startIdx endIdx (fun outLen ->
      let out = Array.zeroCreate outLen

      let today = startIdx - lookbackTotal
      out.[0] <- movingCompareAux today startIdx today cmpF data

      for today in startIdx + 1 .. endIdx do
        let outIdx = today - startIdx
        let prevIdx = out.[outIdx - 1]
        let current =
          //the last extrema is out of the observation range : scan the whole range
          if prevIdx <= today - period then
            movingCompareAux (today-lookbackTotal) today today cmpF data
          //the last extrema is in the observation range : compare with the latest observation
          else
            if cmpF data.[today] data.[prevIdx] then today else prevIdx
        out.[outIdx] <- current

      out
    )

  //=====================================================

  let minIdxLookback period = period - 1

  [<TradingStudy;
    Group("Statistics");
    Title("Minimum index");
    Description("Returns the position of the minimum value within the last n observations");
  >]
  let minIdx period data =
    cmp (<=) period data 

  //=====================================================

  let maxIdxLookback period = period - 1

  [<TradingStudy;
    Group("Statistics");
    Title("Maximum index");
    Description("Returns the position of the maximum value within the last n observations");
  >]
  let maxIdx period data =
    cmp (>=) period data 

  //=====================================================

  let minLookback period = period - 1

  [<TradingStudy;
    Group("Statistics");
    Title("Minimum");
    Description("Returns the minimum value within the last n observations");
    Overlay;
  >]
  let min ([<DefaultValue("14"); Numeric("1","nan","1")>]period) data =
    minIdx period data |> Array.map (Array.get data)

  //=====================================================

  let maxLookback period = period - 1

  [<TradingStudy;
    Group("Statistics");
    Title("Maximum");
    Description("Returns the minimum value within the last n observations");
    Overlay;
  >]
  let max ([<DefaultValue("14"); Numeric("1","nan","1")>]period) data =
    maxIdx period data |> Array.map (Array.get data)

  //=====================================================

  let meanLookback period = period - 1

  [<TradingStudy;
    Group("Statistics");
    Title("Mean");
    Description("Returns the mean value of the last n observations");
    Overlay;
  >]
  let mean period (data:float[]) =
    Study.checkPositiveIntMin1 period
    let lookbackTotal = meanLookback period
    let startIdx = lookbackTotal
    let endIdx = data.Length - 1
    Study.lazyCompute startIdx endIdx (fun outLen ->
      let out = Array.zeroCreate outLen

      let n = float period

      for i in startIdx - lookbackTotal .. startIdx do
        out.[0] <- out.[0] + (data.[i] / n)

      for i in startIdx+1 .. endIdx do
        let outIdx = i - startIdx
        out.[outIdx] <- out.[outIdx-1] + ((data.[i] - data.[i-period])/ n)

      out
    )

  //=====================================================

  let medianLookback period = period - 1

  [<TradingStudy;
    Group("Statistics");
    Title("Median");
    Description("Returns the median value of the last n observations");
    Overlay;
  >]
  let median period (data:float[]) =
    Study.checkPositiveIntMin1 period
    let lookbackTotal = medianLookback period
    let startIdx = lookbackTotal
    let endIdx = data.Length - 1
    Study.lazyCompute startIdx endIdx (fun outLen ->
      let out = Array.zeroCreate outLen

      let buffer = Array.zeroCreate period
      let n = float period

      for today in startIdx - lookbackTotal .. startIdx - 1 do
        buffer.[Study.circularIndex today buffer] <- data.[today]

      for today in startIdx .. endIdx do
        let outIdx = today - startIdx
        buffer.[Study.circularIndex today buffer] <- data.[today]
        out.[outIdx] <- Study.median buffer

      out
    )

  //=====================================================

  let avgDevLookback period = period - 1

  let rec avgDevAux today stop currentMean acc n (data:float[]) =
    if today <= stop then
      let toAdd = abs (data.[today] - currentMean) / n
      avgDevAux (today+1) stop currentMean (acc + toAdd) n data
    else acc

  [<TradingStudy;
    Group("Statistics");
    Title("Average Absolute Deviation");
    Description("Returns theaverage absolute deviation of the last n observations");
  >]
  let avgDev period (data:float[]) =
    Study.checkPositiveIntMin1 period
    let lookbackTotal = avgDevLookback period
    let startIdx = lookbackTotal
    let endIdx = data.Length - 1
    Study.lazyCompute startIdx endIdx (fun outLen ->
      let n = float period
      let trailingIdx = startIdx - lookbackTotal
      data
        |> mean period
        |> Array.mapi (fun i currentMean ->
            let currentTrailingIdx = trailingIdx + i
            avgDevAux currentTrailingIdx (currentTrailingIdx+lookbackTotal) currentMean 0.0 n data
          )
    )

  //=====================================================

  let varLookback period = period - 1

  [<TradingStudy;
    Group("Variance");
    Title("Average Absolute Deviation");
    Description("Returns the variance of the last n observations");
  >]
  let var isSample period (data:float[]) =
    Study.checkPositiveIntMin1 period
    let lookbackTotal = varLookback period
    let startIdx = lookbackTotal
    let endIdx = data.Length - 1
    Study.lazyCompute startIdx endIdx (fun outLen ->
      let meanX = data |> mean period
      let xSquared = Array.map (fun x -> x * x) data
      let meanXSquared = xSquared |> mean period
      if not isSample then
        Array.map2 (fun mxx mx -> mxx - (mx * mx)) meanXSquared meanX
      else
        let n = float period
        let coeff = n / (n - 1.0)
        Array.map2 (fun mxx mx -> coeff * (mxx - (mx * mx))) meanXSquared meanX
    )

  //=====================================================

  let stDevLookback period = period - 1

  [<TradingStudy;
    Group("Statistics");
    Title("Standard Deviation");
    Description("Returns the standard deviation of the last n observations");
  >]
  let stDev isSample period data =
    data
      |> var isSample period
      |> Array.map sqrt

  //=====================================================

  let stErrLookback period = period - 1

  [<TradingStudy;
    Group("Statistics");
    Title("Standard Error");
    Description("Returns the standard error of the last n observations");
  >]
  let stErr period data =
    data
      |> stDev true period
      |> Array.map (
              let p = float period
              let x = sqrt p |> ref
              //Finite population correctino when sample >= 5% population
              let n = Array.length data |> float
              if p >= 0.05 * n then
                x := !x * sqrt ((n-p)/(n-1.0))
              fun std -> std / !x
          )

  //=====================================================

  let skewLookback period = period - 1

  let rec skewAux today stop currentMean currentStDev acc coeff (data:float[]) =
    if today <= stop then
      let toAdd = (data.[today] - currentMean) / currentStDev
      skewAux (today+1) stop currentMean currentStDev (acc + (toAdd**3.0)) coeff data
    else coeff * acc

  [<TradingStudy;
    Group("Statistics");
    Title("Skew");
    Description("Returns the skew of the last n observations");
  >]
  let skew period (data:float[]) =
    Study.checkPositiveIntMin1 period
    let lookbackTotal = varLookback period
    let startIdx = lookbackTotal
    let endIdx = data.Length - 1
    Study.lazyCompute startIdx endIdx (fun outLen ->
      let n = float period
      let coeff = n / ((n - 1.0) * (n - 2.0))
      let trailingIdx = startIdx - lookbackTotal

      let mean = data |> mean period
      let sd = data |> stDev true period

      Array.mapi2 (fun i m s ->
        let tIdx = trailingIdx + i
        data |> skewAux tIdx (tIdx+lookbackTotal) m s 0.0 coeff
      ) mean sd
    )

  //=====================================================

  let kurtLookback period = period - 1

  let rec kurtAux today stop currentMean currentStDev acc coeff toSubstract (data:float[]) =
    if today <= stop then
      let toAdd = (data.[today] - currentMean) / currentStDev
      kurtAux (today+1) stop currentMean currentStDev (acc + (toAdd**4.0)) coeff toSubstract data
    else (coeff * acc) - toSubstract

  [<TradingStudy;
    Group("Statistics");
    Title("Kurtosis");
    Description("Returns the kurtosis of the last n observations");
  >]
  let kurt period (data:float[]) =
    Study.checkPositiveIntMin1 period
    let lookbackTotal = varLookback period
    let startIdx = lookbackTotal
    let endIdx = data.Length - 1
    Study.lazyCompute startIdx endIdx (fun outLen ->
      let n = float period
      let n1 = float lookbackTotal
      let n23 = (n - 2.0) * (n - 3.0)
      let coeff = (n * (n + 1.0)) / (n1 * n23)
      let toSubstract = 3.0 * (n1 * n1) / n23
      let trailingIdx = startIdx - lookbackTotal

      let mean = data |> mean period
      let sd = data |> stDev true period

      Array.mapi2 (fun i m s ->
        let tIdx = trailingIdx + i
        data |> kurtAux tIdx (tIdx+lookbackTotal) m s 0.0 coeff toSubstract
      ) mean sd
    )

  //=====================================================

  let covarLookback period = Math.sumLookback period

  [<TradingStudy;
    Group("Statistics");
    Title("Covariance");
    Description("Returns the covariance of two series over the last n observations");
    MultipleInputSeriesAttribute;
  >]
  let covar period (data1:float[]) data2 =
    Study.checkPositiveIntMin1 period
    Study.checkSameInputLength [data1; data2]
    let lookbackTotal = varLookback period
    let startIdx = lookbackTotal
    let endIdx = data1.Length - 1
    Study.lazyCompute startIdx endIdx (fun outLen ->
      let xsum = data1 |> Math.sum period
      let ysum = data2 |> Math.sum period
      let xySum = Math.mult data1 data2 |> Math.sum period
      let n = float period
      Array.init xySum.Length (fun i -> (xySum.[i] - (xsum.[i] * ysum.[i]) / n) / n)
    )

  //=====================================================

  let correlLookback period = Math.sumLookback period

  [<TradingStudy;
    Group("Statistics");
    Title("Correlation coefficient");
    Description("Returns the correlation coefficient of two series over the last n observations");
    MultipleInputSeriesAttribute;
  >]
  let correl isSample period valueIfDenomZero (data1:float[]) data2 =
    Study.checkPositiveIntMin1 period
    Study.checkSameInputLength [data1; data2]
    let lookbackTotal = varLookback period
    let startIdx = lookbackTotal
    let endIdx = data1.Length - 1
    Study.lazyCompute startIdx endIdx (fun outLen ->
      let xy = covar period data1 data2
      let x = stDev isSample period data1
      let y = stDev isSample period data2
      Array.init xy.Length (fun i ->
        if x.[i] <> 0.0 && y.[i] <> 0.0 then xy.[i] / (x.[i] * y.[i]) else valueIfDenomZero
      )
    )

  //=====================================================

  let linRegLookback period = covarLookback period

  let linRegXs n = Array.init n float

  [<TradingStudy;
    Group("Statistics");
    Title("Linear Regression Slope");
    Description("Returns the slope of the linear regression of the last n observations");
  >]
  let linRegSlope isSample period (data:float[]) =
    Study.checkPositiveIntMin1 period
    let lookbackTotal = linRegLookback period
    let startIdx = lookbackTotal
    let endIdx = data.Length - 1
    Study.lazyCompute startIdx endIdx (fun outLen ->
      let xs = linRegXs data.Length
      let cov = covar period xs data
      let var = var isSample period xs
      Array.map2 ( / ) cov var
    )

  [<TradingStudy;
    Group("Statistics");
    Title("Linear Regression Intercept");
    Description("Returns the intercept of the linear regression of the last n observations");
  >]
  let linRegIntercept isSample period (data:float[]) =
    Study.checkPositiveIntMin1 period
    let lookbackTotal = linRegLookback period
    let startIdx = lookbackTotal
    let endIdx = data.Length - 1
    Study.lazyCompute startIdx endIdx (fun outLen ->
      let xs = linRegXs data.Length
      let xmean = mean period xs
      let ymean = mean period data
      let slopes = linRegSlope isSample period data
      Array.init slopes.Length (fun i -> ymean.[i] - (slopes.[i] * xmean.[i]))
    )   

  [<TradingStudy;
    Group("Statistics");
    Title("Linear Regression");
    Description("Returns the linear regression of the last n observations");
    Overlay;
  >]
  let linReg isSample period (data:float[]) =
    Study.checkPositiveIntMin1 period
    let lookbackTotal = linRegLookback period
    let startIdx = lookbackTotal
    let endIdx = data.Length - 1
    Study.lazyCompute startIdx endIdx (fun outLen ->
      let xs = linRegXs data.Length
      let slopes = linRegSlope isSample period data
      let intercepts = linRegIntercept isSample period data
      Array.init slopes.Length (fun i -> intercepts.[i] + (slopes.[i] * xs.[i + lookbackTotal]))
    )
