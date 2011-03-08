namespace Trading.Studies

module Volatility =

  let tHighLookback = 1

  [<TradingStudy;
    Group("Volatility");
    Title("True High");
    Description("Returns the true high");
    Overlay;
  >]
  let tHigh h (c:float[]) =
    Study.checkHighLow h c
    let lookbackTotal = tHighLookback
    let startIdx = lookbackTotal
    let endIdx = c.Length - 1
    Study.lazyCompute startIdx endIdx (fun outLen ->
      Array.init outLen (fun i -> max c.[i] h.[i + 1])
    )

  let tLowLookback = 1

  [<TradingStudy;
    Group("Volatility");
    Title("True Low");
    Description("Returns the true low");
    Overlay;
  >]
  let tLow l (c:float[]) =
    Study.checkHighLow c l
    let lookbackTotal = tLowLookback
    let startIdx = lookbackTotal
    let endIdx = c.Length - 1
    Study.lazyCompute startIdx endIdx (fun outLen ->
      Array.init outLen (fun i -> min c.[i] l.[i + 1])
    )

  let tRangeLookback = 1

  [<TradingStudy;
    Group("Volatility");
    Title("True Range");
    Description("Returns the true range");
    Overlay;
  >]
  let tRange h l (c:float[]) =
    let lookbackTotal = tRangeLookback
    let startIdx = lookbackTotal
    let endIdx = c.Length - 1
    Study.lazyCompute startIdx endIdx (fun outLen ->
      let th = tHigh h c
      let tl = tLow l c
      Array.map2 ( - ) th tl
    )

  let atrLookback period =
    MA.wilmaLookback period + tRangeLookback

  [<TradingStudy;
    Group("Volatility");
    Title("Average True Range");
    Description("Returns the average true range");
    Overlay;
  >]
  let atr period h l c =
    tRange h l c |> MA.wilma period 

  //John Forman - Technical Analysis of Stock & Commodities - May 2006
  let natrLookback period = atrLookback period

  [<TradingStudy;
    Group("Volatility");
    Title("Normalized Average True Range");
    Description("Returns the normalized average true range");
  >]
  let natr period h l c =
    atr period h l c |> Array.mapi (fun i x ->
      if c.[i + period] <> 0.0 then 100.0 * (x / c.[i + period]) else 0.0
    )     

  //Chaikin Volatility
  let chVolLookback maPeriod rocPeriod =
    MA.emaLookback maPeriod + Osc.rocLookback rocPeriod

  [<TradingStudy;
    Group("Volatility");
    Title("Chaikin Volatility");
    Description("Returns the Chaikin volatility");
  >]
  let chVol maPeriod rocPeriod (h:float[]) l =
    Study.checkHighLow h l
    Array.map2 (fun h l -> h - l) h l
      |> MA.ema maPeriod
      |> Osc.roc true nan rocPeriod

  //Olivier Seban Super Trend
  //See : http://hk-lisse.over-blog.com/article-19983903.html
  [<TradingStudy;
    Group("Volatility");
    Title("Seban Super Trend");
    Description("Returns Seban's Super Trend");
    Overlay;
  >]
  let superTrendLookback period =
    atrLookback period + 1

  let superTrend period devUp devDn high low (close:float[]) =
    Study.checkPositiveIntMin1 period
    Study.checkPositiveReal devUp
    Study.checkPositiveReal devDn
    Study.checkHighLowClose high low close
    let lookbackTotal = superTrendLookback period
    let startIdx = lookbackTotal
    let endIdx = close.Length - 1
    Study.lazyCompute startIdx endIdx (fun outLen ->
      let out = Array.zeroCreate outLen

      let atr = atr period high low close
      let atrLookback = atrLookback period
      let median =Price.med high low

      let mutable trend = 1
      let mutable prevTrend = 1
      let mutable prevUp = median.[startIdx-1] + devUp * atr.[0]
      let mutable prevDown = median.[startIdx-1] - devDn * atr.[0]

      for i in startIdx .. endIdx do
        let atrIdx = i - atrLookback
        let mutable up = median.[i] + devUp * atr.[atrIdx]
        let mutable down = median.[i] - devDn * atr.[atrIdx]

        if close.[i] > prevUp then trend <- 1
        elif close.[i] < prevDown then trend <- -1

        let flag = if trend < 0 && prevTrend > 0 then 1 else 0
        let flagh = if trend > 0 && prevTrend < 0 then 1 else 0

        if trend > 0 && down < prevDown then down <- prevDown
        if trend < 0 && up > prevUp then up <- prevUp

        if flag = 1 then up <- median.[i] + devUp * atr.[atrIdx]
        if flagh = 1 then down <- median.[i] - devDn * atr.[atrIdx]

        out.[i-startIdx] <- if trend = 1 then down else up
        prevTrend <- trend
        prevUp <- up
        prevDown <- down

      out
    )

  let dmAux highPrev lowPrev highToday lowToday=
    let deltaHighs = highToday - highPrev
    let deltaLows = lowPrev - lowToday
    if (deltaHighs = deltaLows) || (deltaHighs < 0.0 && deltaLows < 0.0) then  (0.0, 0.0)
    elif deltaHighs > deltaLows then (0.0, deltaHighs)
    else (deltaLows, 0.0)

  let dmLookback period = MA.wilmaLookback period

  [<TradingStudy;
    Group("Volatility");
    Title("Directional Movement DM");
    Description("Returns the directional movement DM");
    OutputSeriesNames("DM-, DM+");
  >]
  let dm period high (low:float[]) =
    Study.checkPositiveIntMin1 period
    Study.checkHighLow high low
    let lookbackTotal = dmLookback period
    let startIdx = lookbackTotal
    let endIdx = low.Length - 1
    Study.lazyCompute2 startIdx endIdx (fun outLen ->
      let lookback = MA.wilmaLookback period
      //in Wilder's approach to compute the ADX, rather than
      //using the first n values : the first value is assumed
      //to be 0.0, and therefore, only the actual n-1 first values
      //are used in the wilder summations.
      //
      //For instance, the first value of a 5-period wilder summation
      //will be computed as the sum of the first four values of the
      //input series. Other values are computed normally.
      //
      //We therefore create a 0.0-filled array where the first item
      //is never computed, and thus left at 0.0, prior
      let n = outLen + lookback + (* 0.0 adjustment *) 1
      let dmPlus = Array.zeroCreate n
      let dmMinus = Array.zeroCreate n
      let startIdx = startIdx - lookback
      //leave the first value, 0.0, untouched
      for today in startIdx + 1 .. endIdx do
        let prev = today - 1
        let (deltaLows, deltaHighs) = dmAux high.[prev] low.[prev] high.[today] low.[today]
        let outIdx = today - startIdx
        dmMinus.[outIdx] <- deltaLows
        dmPlus.[outIdx] <- deltaHighs

      (Misc.wilSum period dmMinus, Misc.wilSum period dmPlus)
    )

  let diLookback period = dmLookback period + 1

  [<TradingStudy;
    Group("Volatility");
    Title("Directional Movement DI");
    Description("Returns the directional index DI");
    OutputSeriesNames("DI-, DI+");
  >]
  let di period high low (close:float[])  =
    Study.checkPositiveIntMin1 period
    Study.checkHighLowClose high low close
    let lookbackTotal = diLookback period
    let startIdx = lookbackTotal
    let endIdx = close.Length - 1
    Study.lazyCompute2 startIdx endIdx (fun outLen ->
      let diPlus = Array.zeroCreate outLen
      let diMinus = Array.zeroCreate outLen

      let dmMinus, dmPlus = dm period high low

      let summedTRange =
        let tr = tRange high low close
        //Create an array with the same number of items + 1
        let tmp = Array.zeroCreate (tr.Length + 1)
        //copy the true range into this array, starting at i=1,
        //thus leaving the first value at 0.0
        Array.blit tr 0 tmp 1 tr.Length
        //Compute the wilder summation over this expanded array
        Misc.wilSum period tmp

      //Ignore the first DI value to compensate for the 0.0 values
      //that were added for each wilder summation, both in the dm
      //and in the true range.
      //Compute other values normally.
      for i in 1 .. summedTRange.Length - 1 do
        let outIdx = i - 1
        diPlus.[outIdx] <- 100.0 * dmPlus.[i] / summedTRange.[i]
        diMinus.[outIdx] <- 100.0 * dmMinus.[i] / summedTRange.[i]

      (diMinus, diPlus)
    )

  let dxLookback period = diLookback period

  [<TradingStudy;
    Group("Volatility");
    Title("Directional Movement DX");
    Description("Returns the directional movement DX");
  >]
  let dx period h l c =
    let diMinus, diPlus = di period h l c
    Array.map2 (fun m p ->
      let diSum = p + m
      if diSum = 0.0 then 0.0 else abs (p - m) / diSum * 100.0
    ) diPlus diMinus

  let adxLookback dxPeriod adxPeriod =
    dxLookback dxPeriod + MA.wilmaLookback adxPeriod

  [<TradingStudy;
    Group("Volatility");
    Title("Directional Movement ADX");
    Description("Returns the average directional movement ADX");
  >]
  let adx dxPeriod adxPeriod h l (c:float[]) =
    let lookbackTotal = adxLookback dxPeriod adxPeriod
    let startIdx = lookbackTotal
    let endIdx = c.Length - 1
    Study.lazyCompute startIdx endIdx (fun outLen ->
      dx dxPeriod h l c |> MA.wilma adxPeriod
    )

  let adxrLookback dxPeriod adxPeriod adxrPeriod =
    adxLookback dxPeriod adxPeriod + MA.wilmaLookback adxrPeriod

  [<TradingStudy;
    Group("Volatility");
    Title("Directional Movement ADXR");
    Description("Returns the average directional movement rating ADXR");
  >]
  let adxr dxPeriod adxPeriod adxrPeriod h l (c:float[]) =
    let lookbackTotal = adxrLookback dxPeriod adxPeriod adxrPeriod
    let startIdx = lookbackTotal
    let endIdx = c.Length - 1
    Study.lazyCompute startIdx endIdx (fun outLen ->
      adx dxPeriod adxPeriod h l c |> MA.wilma adxrPeriod
    )

  //Parabolic stop and reverse

  let sarLookback = 1

  let isLong startsAsLong (high:float[]) (low:float[]) =
    if startsAsLong <> 0 then
      startsAsLong > 0
    else
        let dmMinus, dmPlus = dmAux high.[0] low.[0] high.[1] low.[1]
        dmPlus >= dmMinus

  //Make sure the value is not too high or too low
  let minLow (low:float[]) idx v =
    if idx > 1 then
      min v low.[idx] |> min low.[idx - 1]
    elif idx = 1 then
      min v low.[idx]
    else
      invalidArg "idx" "idx must be equal or larger than 1"

  let maxHigh (high:float[]) idx v =
    if idx > 1 then
      max v high.[idx] |> max high.[idx - 1]
    elif idx = 1 then
      max v high.[idx]
    else
      invalidArg "idx" "idx must be equal or larger than 1"

  [<TradingStudy;
    Group("Volatility");
    Title("Parabolic SAR - Advanced");
    Description("Returns the parabolic stop and reverse with advanced parameters");
    Overlay;
  >]
  let sarExtended startsAsLong offsetOnReverse aFLongInit aFLongStep aFLongMax aFShortInit aFShortStep aFShortMax (high:float[]) low =
    Study.checkPositiveReal offsetOnReverse
    Study.checkPositiveReal aFLongInit
    Study.checkPositiveReal aFLongStep
    Study.checkPositiveReal aFLongMax
    Study.checkPositiveReal aFShortInit
    Study.checkPositiveReal aFShortStep
    Study.checkPositiveReal aFShortMax
    Study.checkHighLow high low
    let lookbackTotal = sarLookback
    let startIdx = lookbackTotal
    let endIdx = high.Length - 1
    Study.lazyCompute startIdx endIdx (fun outLen ->
      let out = Array.zeroCreate outLen

      //
      // Sanitize arguments
      //
      let aFLongInit = min aFLongInit aFLongMax
      let aFShortInit = min aFShortInit aFShortMax

      let aFLongStep = min aFLongStep (aFLongMax - aFLongInit)
      let aFShortStep = min aFShortStep (aFShortMax - aFShortInit)

      let aFLongMax = max aFLongInit aFLongMax
      let aFShortMax = max aFShortInit aFShortMax

      let mutable aFLong = aFLongInit
      let mutable aFShort = aFShortInit

      let offsetOnReverseToShortCoeff = 1.0 + offsetOnReverse
      let offsetOnReverseToLongCoeff = 1.0 - offsetOnReverse

      //
      // Initialize the algorithm state values
      //
      let mutable isLong = isLong startsAsLong high low
      let mutable ep = 0.0
      let mutable sar = 0.0

      let prev = startIdx - 1
      if isLong then
        ep <- high.[startIdx]
        sar <- low.[prev]
      else
        ep <- low.[startIdx]
        sar <- high.[prev]

      for today in startIdx .. endIdx do
        let outIdx = today - startIdx
        if isLong then
          if low.[today] < sar then
            //init short
            isLong <- false
            sar <- (maxHigh high today ep) * offsetOnReverseToShortCoeff
            //store the sar
            out.[outIdx] <- sar
            //compute next sar
            ep <- low.[today]
            aFShort <- aFShortInit
            sar <- sar + aFShort * (ep - sar)
          else
            //store the sar
            out.[outIdx] <- sar
            //update long
            if high.[today] > ep then
              ep <- high.[today]
              aFLong <- min aFLongMax (aFLong + aFLongStep)
            //compute next long
            let newSar = sar + aFLong * (ep - sar)
            sar <- minLow low today newSar
        else
          if high.[today] > sar then
            //init long
            isLong <- true
            sar <- (minLow low today ep) * offsetOnReverseToLongCoeff
            //store the sar
            out.[outIdx] <- sar
            //compute next sar
            ep <- high.[today]
            aFLong <- aFLongInit
            sar <- sar + aFLong * (ep - sar)
          else
            //store the sar
            out.[outIdx] <- sar
            //update short
            if low.[today] < ep then
              ep <- low.[today]
              aFShort <- min aFShortMax (aFShort + aFShortStep)
            //compute next short
            let newSar = sar + aFShort * (ep - sar)
            sar <- maxHigh high today newSar 

      out
    )

  [<TradingStudy;
    Group("Volatility");
    Title("Parabolic SAR");
    Description("Returns the parabolic stop and reverse");
    Overlay;
  >]
  let sar aFInit aFStep aFMax high low =
    sarExtended 0 0.0 aFInit aFStep aFMax aFInit aFStep aFMax high low

  let maxPeriod p1 p2 p3 =
    max p1 p2 |> max p3

  let largestLookback p1 p2 p3 =
    maxPeriod p1 p2 p3 |> MA.smaLookback

  let ultOscLookback p1 p2 p3 =
    largestLookback p1 p2 p3 + tRangeLookback

  [<TradingStudy;
    Group("Volatility");
    Title("Ultimate Oscillator");
    Description("Returns the ultimate oscillator");
  >]
  let ultOsc p1 p2 p3 high low (close:float[]) =
    Study.checkPositiveIntMin1 p1
    Study.checkPositiveIntMin1 p2
    Study.checkPositiveIntMin1 p3
    Study.checkHighLowClose high low close
    let lookbackTotal = ultOscLookback p1 p2 p3
    let startIdx = lookbackTotal
    let endIdx = close.Length - 1
    Study.lazyCompute startIdx endIdx (fun outLen ->
      let periods = [|p1; p2; p3|] |> Array.sort
      let p1 = periods.[0] //shortest period
      let p2 = periods.[1]
      let p3 = periods.[2] //longest period

      //make true range and delta start so that the slowest
      //moving average will yield a result at startIdx
      let trueRange = tRange high low close
      let delta =
        let tlow = tLow low close
        let offset = close.Length - tlow.Length
        Array.init tlow.Length (fun i -> close.[i+offset] - tlow.[i])

      let a1 = Math.sum p1 delta
      let a2 = Math.sum p2 delta
      let a3 = Math.sum p3 delta

      let b1 = Math.sum p1 trueRange
      let b2 = Math.sum p2 trueRange
      let b3 = Math.sum p3 trueRange

      let offset13 = a1.Length - a3.Length
      let offset23 = a2.Length - a3.Length

      let alpha = 100.0 / 7.0

      Array.init outLen (fun i ->
        let a = a1.[i+offset13] / b1.[i+offset13]
        let b = a2.[i+offset23] / b2.[i+offset23]
        let c = a3.[i] / b3.[i]
        (4.0 * a + 2.0 * b + c) * alpha
      )
    )
