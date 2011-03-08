namespace Trading.Studies

//
// John Ehlers - mostly from Cybernetic Analysis for stocks and futures (2004)
//
open Trading.Studies

module Ehlers =
  //helpers

  let CURRENT_BAR_6 = 5

  let arrayGet (data:float[]) i =
    if i >= 0 then data.[i] else 0.0

  let quadrature (data:float[]) i =
    let a = arrayGet data i
    let b = arrayGet data (i-2)
    let c = arrayGet data (i-4)
    let d = arrayGet data (i-6)
    0.0962*a + 0.5769*b - 0.5769*c - 0.0962*d

  let inPhase (data:float[]) i = arrayGet data (i-3)

  //Trigger line used for any study
  let triggerLookback = 1

  [<TradingStudy;
    Group("Misc");
    Title("Trigger Line");
    Description("Returns a trigger line to generate cross-over signals");
    Overlay;
  >]
  let trigger (data:float[]) =
    Array.init (Array.length data - 1) (fun i -> data.[i+1])

  //Fisher transform
  let fisherTLookback period =
    Osc.stochFastKLookback period

  [<TradingStudy;
    Group("Cycle");
    Title("Ehlers Fisher Transform");
    Description("Returns the Ehlers Fisher transform of a series - from Cybernetic Analysis for stocks and futures (2004)");
  >]
  let fisherT period (h:float[]) l c =
    Study.checkPositiveIntMin1 period
    Study.checkHighLowClose h l c
    let lookbackTotal = fisherTLookback period
    let startIdx = lookbackTotal
    let endIdx = h.Length - 1
    Study.lazyCompute startIdx endIdx (fun outLen ->
      let out = Array.zeroCreate outLen

      let hh = Stat.max period h
      let ll = Stat.min period l

      let mutable normalizedRatio = 0.0
      let mutable fisher = 0.0
      for i in startIdx .. endIdx do
        let outIdx = i - startIdx
        let fastKIdx = outIdx
        let fastK =
          let denom = hh.[fastKIdx] - ll.[fastKIdx]
          if denom <> 0.0 then (c.[i] - ll.[fastKIdx]) / denom else 0.0
        normalizedRatio <- (fastK-0.5) + 0.5*normalizedRatio |> min 0.9999 |> max (-0.9999)
        fisher <- 0.25*log((1.0+normalizedRatio)/(1.0-normalizedRatio)) + 0.5*fisher

        out.[outIdx] <- fisher       

      out
    )

  //===============================
  //
  // Instantaneous trendline
  //
  //===============================

  let itrendLookback = MA.trimaLookback 3

  [<TradingStudy;
    Group("Smoothing");
    Title("Ehlers Instantaneous Trendline");
    Description("Returns the Ehlers instantaneous trendline - from Cybernetic Analysis for stocks and futures (2004)");
    Overlay;
  >]
  let itrend alpha (data:float[]) =
    Study.checkPositiveReal alpha
    let lookbackTotal = itrendLookback
    let startIdx = lookbackTotal
    let endIdx = data.Length - 1
    Study.lazyCompute startIdx endIdx (fun outLen ->
      let out = Array.zeroCreate outLen

      let trimaEndIdx = min endIdx CURRENT_BAR_6
      for i in startIdx .. trimaEndIdx do
        let outIdx = i - startIdx
        out.[outIdx] <- 0.25 * (data.[i] + 2.0*data.[i-1] + data.[i-2])

      if endIdx > trimaEndIdx then
        let a = alpha * alpha
        let b = 1.0 - alpha
        let c = alpha - (0.25 * a)
        let d = 0.5 * a
        let e = alpha - (0.75 * a)
        let f = 2.0 * b
        let g = b * b

        for i in trimaEndIdx+1 .. endIdx do
          let outIdx = i - startIdx
          out.[outIdx] <- c*data.[i] + d*data.[i-1] - e*data.[i-2] + f*out.[outIdx-1] - g*out.[outIdx-2]

      out
    )

  //===============================
  //
  // Cyber cycle
  //
  //===============================

  let ccycleLookback = 2

  let ccycleKnownSmooth startIdx endIdx alpha (data:float[]) (smooth:float[]) =
    Study.lazyCompute startIdx endIdx (fun outLen ->
      let out = Array.zeroCreate outLen

      let trimaEndIdx = min endIdx CURRENT_BAR_6
      for i in startIdx .. trimaEndIdx do
        let outIdx = i - startIdx
        out.[outIdx] <- 0.25 * (data.[i] - 2.0*data.[i-1] + data.[i-2])

      if endIdx > trimaEndIdx then
        let a = 1.0 - alpha
        let b = 1.0 - 0.5 * alpha
        let c = b * b
        let d = 2.0 * a
        let e = a * a

        for i in trimaEndIdx+1 .. endIdx do
          let outIdx = i - startIdx
          //smooth = 4-bar triangular mov avg
          //first output values = 3-bar triangular mov avg
          //--> smooth has a one-bar lag vs output values
          let smoothIdx = outIdx - 1
          let x = smooth.[smoothIdx] - 2.0*smooth.[smoothIdx-1] + smooth.[smoothIdx-2]
          out.[outIdx] <- c*x + d*out.[outIdx-1] - e*out.[outIdx-2]
      out
    )

  [<TradingStudy;
    Group("Cycle");
    Title("Ehlers Cyber Cycle");
    Description("Returns the Ehlers cyber cycle - from Cybernetic Analysis for stocks and futures (2004)");
  >]
  let ccycle alpha (data:float[]) =
    Study.checkPositiveReal alpha
    let lookbackTotal = ccycleLookback
    let startIdx = lookbackTotal
    let endIdx = data.Length - 1
    Study.lazyCompute startIdx endIdx (fun outLen ->
      ccycleKnownSmooth startIdx endIdx alpha data (MA.trima 4 data)
    )

  //===============================
  //
  //Center of gravity
  //
  //===============================

  let cgLookback period = period - 1

  //used for Centrer of gravity && adaptive ceter of gravity
  let cgAux wsum sum delta =
    -(wsum / sum) + delta

  [<TradingStudy;
    Group("Cycle");
    Title("Ehlers Center of Gravity");
    Description("Returns the Ehlers center of gravity - from Cybernetic Analysis for stocks and futures (2004)");
  >]
  let cg period (data:float[]) =
    Study.checkPositiveIntMin1 period
    let lookbackTotal = cgLookback period
    let startIdx = lookbackTotal
    let endIdx = data.Length - 1
    Study.lazyCompute startIdx endIdx (fun outLen ->
      let out = Array.zeroCreate outLen

      let mutable wsum = 0.0
      let mutable sum = 0.0

      let n = float period
      let delta = (n + 1.0) / 2.0

      let mutable w = n
      for i in startIdx - lookbackTotal .. startIdx do
        wsum <- wsum + (w * data.[i])
        sum <- sum + data.[i]
        w <- w - 1.0

      out.[0] <- cgAux wsum sum delta

      for i in startIdx+1 .. endIdx do
        let outIdx = i - startIdx
        sum <- sum + data.[i] - data.[i-period]
        wsum <- wsum + sum - (n * data.[i-period])
        out.[outIdx] <- cgAux wsum sum delta

      out
    )

  //===============================
  //
  //Relative vigor index
  //
  //===============================

  let rviLookback period =
    MA.trimaLookback 4 + Math.sumLookback period

  let rviAux x y =
    Array.map2 (-) x y |> MA.trima 4 

  [<TradingStudy;
    Group("Cycle");
    Title("Ehlers Relative Vigor Index");
    Description("Returns the Ehlers relative vigor index - from Cybernetic Analysis for stocks and futures (2004)");
  >]
  let rvi period o h l (c:float[]) =
    Study.checkPositiveIntMin1 period
    Study.checkOpenHighLowClose o h l c
    let lookbackTotal = rviLookback period
    let startIdx = lookbackTotal
    let endIdx = c.Length - 1
    Study.lazyCompute startIdx endIdx (fun outLen ->
      let co = rviAux c o |> Math.sum period
      let hl = rviAux h l |> Math.sum period
      Array.map2 (fun co hl -> if hl <> 0.0 then co / hl else 0.0) co hl
    )

  //===============================
  //
  // Stochastization & Fisherization
  //
  //===============================

  let stochFastKLookback period = Osc.stochFastKLookback period

  [<TradingStudy;
    Group("Cycle");
    Title("Ehlers Fast K");
    Description("Returns the Ehlers fast K of a series - from Cybernetic Analysis for stocks and futures (2004)");
  >]
  let stochFastK period data =
    Osc.stochFastK period data data data

  let stochFastLookback fastKPeriod slowKPeriod =
    Osc.stochFastLookback fastKPeriod MA.Weighted slowKPeriod

  [<TradingStudy;
    Group("Cycle");
    Title("Ehlers Fast Stochastics");
    Description("Returns the Ehlers fast K and slow K of a series - from Cybernetic Analysis for stocks and futures (2004)");
  >]
  let stochFast fastKPeriod slowKPeriod data =
    Osc.stochFast fastKPeriod MA.Weighted slowKPeriod data data data

  let stochSlowLookback fastKPeriod slowKPeriod slowDPeriod =
    Osc.stochSlowLookback fastKPeriod MA.Weighted slowKPeriod MA.Weighted slowDPeriod

  [<TradingStudy;
    Group("Cycle");
    Title("Ehlers Slow Stochastics");
    Description("Returns the Ehlers fast K, slow K and slow D of a series - from Cybernetic Analysis for stocks and futures (2004)");
  >]
  let stochSlow fastKPeriod slowKPeriod slowDPeriod data =
    Osc.stochSlow fastKPeriod MA.Weighted slowKPeriod MA.Weighted slowDPeriod data data data

  [<TradingStudy;
    Group("Cycle");
    Title("Ehlers Fisher Transform");
    Description("Same as fisherT : returns the Ehlers Fisher transform - from Cybernetic Analysis for stocks and futures (2004)");
  >]
  let fisher data = Signal.fisherT data  

  let fisherLookback = Signal.fisherTLookback

  //===============================
  //
  //Dominant Cycle Period
  //
  //===============================

  let dcpSmoothLookback = MA.trimaLookback 4

  let dcpQ1Lookback = 6

  let dcpDPLookback = 1 

  let dcpMedianDeltaLookback = 4     

  let dcpLookback =
    ccycleLookback + dcpQ1Lookback + dcpDPLookback + dcpMedianDeltaLookback

  let dcpFullAux alpha (ccycle:float[]) =
    let startIdx = 0
    let endIdx = ccycle.Length - 1
    Study.lazyCompute startIdx endIdx (fun outLen ->
      let out = Array.zeroCreate outLen

      let mutable prevQ1 = 0.0
      let mutable prevI1 = 0.0
      let mutable q1 = 0.0
      let mutable i1 = 0.0
      let mutable deltaPhase = 0.0
      let mutable medianDelta = Array.zeroCreate 5
      let mutable dc = 0.0
      let mutable instPeriod = 0.0
      let mutable period = 0.0

      for i in 0 .. ccycle.Length - 1 do
        prevQ1 <- q1
        prevI1 <- i1
        q1 <- quadrature ccycle i * (0.5 + 0.08*instPeriod)
        i1 <- inPhase ccycle i
        deltaPhase <-
          if (q1 <> 0.0 && prevQ1 <> 0.0) then
            let re = i1/q1 - prevI1/prevQ1
            let im = 1.0 + (i1*prevI1)/(q1*prevQ1)
            re/im
          else
            0.0
        medianDelta.[Study.circularIndex i medianDelta] <- deltaPhase
        let median = Study.median medianDelta |> max 0.1 |> min 1.1
        dc <- if median = 0.0 then 15.0 else 6.28318/median + 0.5
        instPeriod <- 0.33*dc + 0.67*instPeriod
        period <- 0.15*instPeriod + 0.85*period

        out.[i] <- period

      out
    )

  [<TradingStudy;
    Group("Cycle");
    Title("Ehlers Dominant Cycle Period");
    Description("Returns the Ehlers dominant cycle period - from Cybernetic Analysis for stocks and futures (2004)");
  >]
  let dcp alpha (data:float[]) =
    (ccycle alpha data |> dcpFullAux alpha).[dcpLookback-ccycleLookback..]

  //===============================
  //
  //Adaptive Cyber Cycle
  //
  //===============================

  let accycleLookback = dcpLookback

  [<TradingStudy;
    Group("Cycle");
    Title("Ehlers Adaptive Cyber Cycle");
    Description("Returns the Ehlers adaptive cyber cycle - from Cybernetic Analysis for stocks and futures (2004) -
    Adaptive indicators rely on the Ehlers dominant cycle period rather than on a fixed period");
  >]
  let accycle alpha (data:float[]) =
    Study.checkPositiveReal alpha
    let lookbackTotal = accycleLookback
    let startIdx = lookbackTotal
    let endIdx = data.Length - 1
    Study.lazyCompute startIdx endIdx (fun outLen ->
      let out = Array.zeroCreate outLen

      //smooth is also computed by ccycle below
      //but we need it to compute the adaptive values.

      //Besides since the formula is recursive, we also need
      //the values which are used to seed the first dominant cycle period
      //value, but which are not returned by the indicator.
      //Hence, we need to reuse the whole dominant cycle period computation.

      let smooth = MA.trima 4 data
      let ccycle = ccycleKnownSmooth ccycleLookback endIdx alpha data smooth
      let dcpFull = dcpFullAux alpha ccycle

      //adaptive output values
      let mutable twoAgo = (data.[4] - 2.0*data.[3] + data.[2]) / 4.0
      let mutable oneAgo = (data.[5] - 2.0*data.[4] + data.[3]) / 4.0
      let mutable current = 0.0

      let ccycleStartIdx = startIdx - ccycleLookback

      //The cyber cycle formula gets recursive after
      //at its 5th value (ccycle_idx=6) - or currentBar=7.
      for i in 4 .. ccycle.Length - 1 do
        //we create a coefficient based on the full dominant cycle period
        let beta = 2.0 / (dcpFull.[i] + 1.0)
        let a = 1.0 - beta
        let b = 1.0 - 0.5 * beta
        let c = b * b
        let d = 2.0 * a
        let e = a * a

        //smooth returns one bar less than the cyber cycle
        let smoothIdx = i - 1
        let x = smooth.[smoothIdx] - 2.0*smooth.[smoothIdx-1] + smooth.[smoothIdx-2]
        current <- c*x + d*oneAgo - e*twoAgo
        twoAgo <- oneAgo
        oneAgo <- current

        if i >= ccycleStartIdx then
          out.[i - ccycleStartIdx] <- current

      out
    )

  //===============================
  //
  //Adaptive Center of Gravity
  //
  //===============================

  let acgLookback = dcpLookback

  [<TradingStudy;
    Group("Cycle");
    Title("Ehlers Adaptive Center of Gravity");
    Description("Returns the Ehlers adaptive center of gravity - from Cybernetic Analysis for stocks and futures (2004) -
    Adaptive indicators rely on the Ehlers dominant cycle period rather than on a fixed period");
  >]
  let acg alpha (data:float[]) =
    let endIdx = data.Length - 1
    dcp alpha data |> Array.mapi (fun dcpIdx period ->
      //the integer portion of the half dominant cycle period is used
      let intPart = period / 2.0 |> floor
      let intPeriod = int intPart
      let delta = (intPart + 1.0) / 2.0

      let mutable wsum = 0.0
      let mutable sum = 0.0      

      let dataIdx = dcpIdx + dcpLookback

      for i in dataIdx .. -1 .. max 0 (dataIdx - intPeriod + 1) do
        let w = dataIdx - i + 1 |> float
        wsum <- wsum + (w * data.[i])
        sum <- sum + data.[i]

      cgAux wsum sum delta
    )

  //===============================
  //
  //Adaptive Relative vigor index
  //
  //===============================

  let arviDcpAverageLookback =
    //in fact, it's not a real weighted moving average, but the formula
    //is pretty resembling
    MA.wmaLookback 5 

  let arviLookback =
    dcpLookback + arviDcpAverageLookback

  [<TradingStudy;
    Group("Cycle");
    Title("Ehlers Adaptive Relative Vigor Index");
    Description("Returns the Ehlers adaptive relative vigor index - from Cybernetic Analysis for stocks and futures (2004) -
    Adaptive indicators rely on the Ehlers dominant cycle period rather than on a fixed period");
  >]
  let arvi alpha data o h l (c:float[]) =
    Study.checkPositiveReal alpha
    Study.checkOpenHighLowClose o h l c
    let lookbackTotal = arviLookback
    let startIdx = lookbackTotal
    let endIdx = c.Length - 1
    Study.lazyCompute startIdx endIdx (fun outLen ->
      let out = Array.zeroCreate outLen

      let co = rviAux c o
      let hl = rviAux h l

      //Dominant cycle period is smoothed contrary to other adaptive indicators
      //the smoothing looks like a weighted ma, but the denominator is larger
      //than the sum of the weights so that the study takes into account the
      //integer portion of the half dominant cycle period.
      //
      //The values taken into account do not follow each other in the average :
      //we have 4 values spaced over a 5-bar interval.
      //
      //More weight is given to the most recent bar.
      let smoothedPeriod =
        let dcp = dcp alpha data
        Array.init (dcp.Length - 4) (fun i ->
          (dcp.[i] + 2.0*dcp.[i+1] + 3.0*dcp.[i+3] + 4.0*dcp.[i+4]) / 20.0 |> int
        )

      let mutable v1 = 0.0
      let mutable v2 = 0.0
      for i in startIdx .. endIdx do
        let valueIdx = i - dcpSmoothLookback
        let periodIdx = i - startIdx
        let outIdx = periodIdx
        v1 <- 0.0
        v2 <- 0.0
        for j in valueIdx .. -1 .. max 0 (valueIdx - smoothedPeriod.[periodIdx]) do
          v1 <- v1 + co.[j]
          v2 <- v2 + hl.[j]
        out.[outIdx] <- if v2 <> 0.0 then v1 / v2 else 0.0

      out

    )

  //===============================
  //
  // Sinewave indicator
  //
  //===============================

  let sinewaveLookback = dcpLookback

  [<TradingStudy;
    Group("Cycle");
    Title("Ehlers Sinewave");
    Description("Returns the Ehlers sinewave - from Cybernetic Analysis for stocks and futures (2004)");
    OutputSeriesNames("sine, lead");
  >]
  let sinewave alpha (data:float[]) =
    Study.checkPositiveReal alpha
    let lookbackTotal = sinewaveLookback
    let startIdx = lookbackTotal
    let endIdx = data.Length - 1
    Study.lazyCompute2 startIdx endIdx (fun outLen ->
      let sine = Array.zeroCreate outLen
      let sineLead = Array.zeroCreate outLen

      let cc = ccycle alpha data
      let dcp = dcp alpha data

      let mutable re = 0.0
      let mutable im = 0.0
      let offset = sinewaveLookback - ccycleLookback
      let mutable dcPhase = 0.0
      for periodIdx in 0 .. dcp.Length - 1 do
        let dcperiod = floor dcp.[periodIdx]
        re <- 0.0
        im <- 0.0

        let ccIdx = periodIdx + offset
        for i in ccIdx .. -1 .. max 0 (ccIdx - int dcperiod + 1) do
          let theta =
            let x = ccIdx - i |> float
            360.0 * x / dcperiod |> Study.degToRad
          re <- re + cc.[i] * sin theta
          im <- im + cc.[i] * cos theta

        if abs im > 0.001 then dcPhase <- atan(re/im) |> Study.radToDeg
        elif re > 0.0 then dcPhase <- 90.0
        else dcPhase <- -90.0

        dcPhase <- dcPhase + 90.0 

        if im < 0.0 then dcPhase <- dcPhase + 180.0
        if dcPhase > 315.0 then dcPhase <- dcPhase - 360.0

        let outIdx = periodIdx
        sine.[outIdx] <- dcPhase |> Study.degToRad |> sin
        sineLead.[outIdx] <- dcPhase + 45.0 |> Study.degToRad |> sin

      sine, sineLead
    )    

  //===============================
  //
  //Adaptive momentum
  //
  //===============================

  let amomLookback = dcpLookback

  [<TradingStudy;
    Group("Cycle");
    Title("Ehlers Adaptive Momentum");
    Description("Returns the Ehlers adaptive momentum - from Cybernetic Analysis for stocks and futures (2004) -
    Adaptive indicators rely on the Ehlers dominant cycle period rather than on a fixed period");
  >]
  let amom cutoff alpha (data:float[]) =
    Study.checkPositiveReal alpha
    let lookbackTotal = amomLookback
    let startIdx = lookbackTotal
    let endIdx = data.Length - 1
    Study.lazyCompute startIdx endIdx (fun outLen ->
      let out = Array.zeroCreate outLen

      //smooth is also computed by ccycle below
      //but we need it to compute the adaptive values... argh !

      //Besides since the formula is recursive, we also need
      //the values which are used to seed the first dominant cycle period
      //value, but which are not returned by the indicator.
      //Hence, we need to reuse the whole dominant cycle period computation.

      let smooth = MA.trima 4 data
      let ccycle = ccycleKnownSmooth ccycleLookback endIdx alpha data smooth
      let dcpFull = dcpFullAux alpha ccycle

      //adaptive mom constants
      let a = exp(-System.Math.PI / cutoff)
      let b = 2.0 * a * cos(1.7318 * 180.0 / cutoff |> Study.degToRad)
      let c = a * a
      let coef2 = b + c
      let coef3 = -(c + b*c)
      let coef4 = c * c
      let coef1 = 1.0 - coef2 - coef3 - coef4

      let amomStartIdx = startIdx - ccycleLookback

      let computeValue1 i =
        let dataIdx = i + ccycleLookback
        let dataIdxLag = dataIdx - (int dcpFull.[i] - 1)
        if dataIdxLag >= 0 then
          data.[dataIdx] - data.[dataIdxLag]
        else
          0.0

      //adaptive mom
      let mutable value1 = computeValue1 0
      //adaptive output values
      let mutable threeAgo = 0.0
      let mutable twoAgo = 0.0
      let mutable oneAgo = 0.0
      let mutable current = value1

      for i in 1 .. ccycle.Length - 1 do
        //adaptive momentum
        let value1 = computeValue1 i

        //Algo says currentBar < 4
        //However since the first ccycle bar is at currentBar = 3
        //there only needs one such definition, which is done prior to looping
        current <- coef1*value1 + coef2*oneAgo + coef3*twoAgo + coef4*threeAgo

        threeAgo <- twoAgo
        twoAgo <- oneAgo
        oneAgo <- current

        //The cyber cycle formula gets recursive after
        //at its 5th value (ccycle_idx=6) - or currentBar=7.
        if i >= amomStartIdx then
          out.[i - amomStartIdx] <- current

      out
    )

  //===============================
  //
  // Two-Pole Butterworth filter
  //
  //===============================

  let twoPbfLookback = 0

  let twoPoleAux coef1 coef2 coef3 (data:float[]) =
    let out = Array.zeroCreate data.Length
    out.[0] <- data.[0]

    //there could be only one value in the input data.
    if data.Length > 1 then
      out.[1] <- data.[1]

    if data.Length > 2 then
      let smooth = MA.trima 3 data
      let recursionStartIdx = MA.trimaLookback 3
      let endIdx = data.Length - 1
      for i in recursionStartIdx .. endIdx do
        //The tradestation formula is inconsistent with the e-signal version
        //the latter seems more in-line with previous formulas, and we therefore use it
        out.[i] <- coef1*smooth.[i-recursionStartIdx] + coef2*out.[i-1] + coef3*out.[i-2]
    out

  [<TradingStudy;
    Group("Cycle");
    Title("Ehlers Two-Pole Butterworth Filter");
    Description("Returns the Ehlers two-pole Butterworth filter - from Cybernetic Analysis for stocks and futures (2004)");
  >]
  let twoPbf period (data:float[]) =
    Study.checkPositiveIntMin1 period
    let lookbackTotal = twoPbfLookback
    let startIdx = lookbackTotal
    let endIdx = data.Length - 1
    Study.lazyCompute startIdx endIdx (fun outLen ->
      //constants
      let period = float period
      let a = exp(-1.414*System.Math.PI / period)
      let b = 2.0 * a * cos(1.414 * 180.0 / period |> Study.degToRad)
      let coef2 = b
      let coef3 = (-a) * a
      let coef1 = (1.0 - b + a*a) / 4.0

      twoPoleAux coef1 coef2 coef3 data
    )

  //===============================
  //
  // Three-Pole Butterworth filter
  //
  //===============================

  let threePbfLookback = 0

  let threePoleAux coef1 coef2 coef3 coef4 (data:float[]) =
    let out = Array.zeroCreate data.Length
    out.[0] <- data.[0]

    //there could be only one value in the input data.
    if data.Length > 1 then
      out.[1] <- data.[1]

    if data.Length > 2 then
      out.[2] <- data.[2]

    if data.Length > 2 then
      let smooth = MA.trima 4 data
      let recursionStartIdx = MA.trimaLookback 4
      let endIdx = data.Length - 1
      for i in recursionStartIdx .. endIdx do
        out.[i] <- coef1*smooth.[i-recursionStartIdx] + coef2*out.[i-1] + coef3*out.[i-2] + coef4*out.[i-3]

    out

  [<TradingStudy;
    Group("Cycle");
    Title("Ehlers Three-Pole Butterworth Filter");
    Description("Returns the Ehlers three-pole Butterworth filter - from Cybernetic Analysis for stocks and futures (2004)");
  >]
  let threePbf period (data:float[]) =
    Study.checkPositiveIntMin1 period
    let lookbackTotal = threePbfLookback
    let startIdx = lookbackTotal
    let endIdx = data.Length - 1
    Study.lazyCompute startIdx endIdx (fun outLen ->
      //constants
      let period = float period
      let a = exp(-1.414*System.Math.PI / period)
      let b = 2.0 * a * cos(1.414 * 180.0 / period |> Study.degToRad)
      let c = a * a
      let coef2 = b + c
      let coef3 = -(c + b*c)
      let coef4 = c * c
      let coef1 = (1.0 - b + c) * (1.0 - c) / 8.0

      threePoleAux coef1 coef2 coef3 coef4 data
    )

  //===============================
  //
  // Two-Pole Super Smoother
  //
  //===============================

  let twoPssLookback = 0

  [<TradingStudy;
    Group("Cycle");
    Title("Ehlers Two-Pole Super Smoother");
    Description("Returns the Ehlers two-pole super smoother - from Cybernetic Analysis for stocks and futures (2004)");
  >]
  let twoPss period (data:float[]) =
    Study.checkPositiveIntMin1 period
    let lookbackTotal = twoPssLookback
    let startIdx = lookbackTotal
    let endIdx = data.Length - 1
    Study.lazyCompute startIdx endIdx (fun outLen ->
      //constants
      let period = float period
      let a = exp(-1.414*System.Math.PI / period)
      let b = 2.0 * a * cos(1.414 * 180.0 / period |> Study.degToRad)
      let coef2 = b
      let coef3 = (-a) * a
      let coef1 = (1.0 - b + a*a) / 4.0

      twoPoleAux coef1 coef2 coef3 data
    )

  //===============================
  //
  // Three-Pole Super Smoother
  //
  //===============================

  let threePssLookback = 0

  [<TradingStudy;
    Group("Cycle");
    Title("Ehlers Three-Pole Super Smoother");
    Description("Returns the Ehlers three-pole super smoother - from Cybernetic Analysis for stocks and futures (2004)");
  >]
  let threePss period (data:float[]) =
    Study.checkPositiveIntMin1 period
    let lookbackTotal = threePssLookback
    let startIdx = lookbackTotal
    let endIdx = data.Length - 1
    Study.lazyCompute startIdx endIdx (fun outLen ->
      //constants
      let period = float period
      let a = exp(-1.414*System.Math.PI / period)
      let b = 2.0 * a * cos(1.414 * 180.0 / period |> Study.degToRad)
      let c = a * a
      let coef2 = b + c
      let coef3 = -(c + b*c)
      let coef4 = c * c
      let coef1 = (1.0 - b + c) * (1.0 - c) / 8.0

      threePoleAux coef1 coef2 coef3 coef4 data
    )

  //===============================
  //
  // laguerre Filter
  //
  //===============================

  let laguerreLookback = 0

  [<TradingStudy;
    Group("Cycle");
    Title("Ehlers Laguerre Filter");
    Description("Returns the Ehlers Laguerre filter - from Cybernetic Analysis for stocks and futures (2004)");
  >]
  let laguerre gamma (data:float[]) =
    Study.checkPositiveReal gamma
    let lookbackTotal = laguerreLookback
    let startIdx = lookbackTotal
    let endIdx = data.Length - 1
    Study.lazyCompute startIdx endIdx (fun outLen ->
      let out = Array.zeroCreate outLen

      let alpha = 1.0 - gamma
      let beta = -gamma

      let mutable prevL0 = 0.0
      let mutable prevL1 = 0.0
      let mutable prevL2 = 0.0
      let mutable prevL3 = 0.0

      let mutable L0 = 0.0
      let mutable L1 = 0.0
      let mutable L2 = 0.0
      let mutable L3 = 0.0

      for i in 0 .. endIdx do
        L0 <- alpha*data.[i] + gamma*L0
        L1 <- beta*L0 +  prevL0 + gamma*L1
        L1 <- beta*L1 +  prevL1 + gamma*L2
        L1 <- beta*L2 +  prevL2 + gamma*L3

        prevL0 <- L0
        prevL1 <- L1
        prevL2 <- L2
        prevL3 <- L3

        if i >= startIdx then
          out.[i-startIdx] <- (L0 + 2.0*L1 + 2.0*L2 + L3) / 6.0

      out
    )

  //===============================
  //
  // Laguerre RSI
  //
  //===============================

  let laguerreRsiLookback = 0

  let laguerreRsiAux x y (cu, cd) =
    let delta = x - y
    if delta > 0.0 then (cu + delta, cd) else (cu, cd - delta)

  [<TradingStudy;
    Group("Cycle");
    Title("Ehlers Laguerre RSI");
    Description("Returns the Ehlers Laguerre RSI (relative strength index) - from Cybernetic Analysis for stocks and futures (2004)");
  >]
  let laguerreRsi gamma (data:float[]) =
    Study.checkPositiveReal gamma
    let lookbackTotal = laguerreRsiLookback
    let startIdx = lookbackTotal
    let endIdx = data.Length - 1
    Study.lazyCompute startIdx endIdx (fun outLen ->
      let out = Array.zeroCreate outLen

      let alpha = 1.0 - gamma
      let beta = -gamma

      let mutable prevL0 = 0.0
      let mutable prevL1 = 0.0
      let mutable prevL2 = 0.0
      let mutable prevL3 = 0.0

      let mutable L0 = 0.0
      let mutable L1 = 0.0
      let mutable L2 = 0.0
      let mutable L3 = 0.0

      for i in 0 .. endIdx do
        L0 <- alpha*data.[i] + gamma*L0
        L1 <- beta*L0 +  prevL0 + gamma*L1
        L2 <- beta*L1 +  prevL1 + gamma*L2
        L3 <- beta*L2 +  prevL2 + gamma*L3

        prevL0 <- L0
        prevL1 <- L1
        prevL2 <- L2
        prevL3 <- L3

        if i >= startIdx then
          let CU, CD =
            (0.0, 0.0)
              |> laguerreRsiAux L0 L1
              |> laguerreRsiAux L1 L2
              |> laguerreRsiAux L2 L3
          let denom = CU + CD
          out.[i-startIdx] <- if denom <> 0.0 then CU / denom else 0.0

      out
    )

  //===============================
  //
  // Leading indicator
  //
  //===============================

  let leadLookback = 1

  [<TradingStudy;
    Group("Cycle");
    Title("Ehlers Leading Indicator");
    Description("Returns the Ehlers leading indicator - from Cybernetic Analysis for stocks and futures (2004)");
  >]
  let lead alpha1 alpha2 (data:float[]) =
    Study.checkPositiveReal alpha1
    Study.checkPositiveReal alpha2
    let lookbackTotal = leadLookback
    let startIdx = lookbackTotal
    let endIdx = data.Length - 1
    Study.lazyCompute startIdx endIdx (fun outLen ->
      let out = Array.zeroCreate outLen

      let a = alpha1 - 2.0
      let b = 1.0 - alpha1
      let c = 1.0 - alpha2

      let mutable lead = 0.0
      let mutable netLead = 0.0

      for i in startIdx .. endIdx do
        lead <- 2.0*data.[i] + a*data.[i-1] + b*lead
        netLead <- alpha2*lead + c*netLead
        out.[i-startIdx] <- netLead

      out
    )

  //===============================
  //
  // Fractal Moving Average
  //
  //===============================

  let framaLookback period = Stat.maxLookback period

  [<TradingStudy;
    Group("Smoothing");
    Title("Ehlers Fractal Moving Average");
    Description("Returns the Ehlers fractal moving average");
    Overlay;
  >]
  let frama period (data:float[]) =
    Study.checkPositiveIntMin1 period
    if period % 2 <> 0 then
      raise <| BadParam "period must be even"
    let lookbackTotal = framaLookback period
    let startIdx = lookbackTotal
    let endIdx = data.Length - 1
    Study.lazyCompute startIdx endIdx (fun outLen ->
      let out = Array.zeroCreate outLen
      let n = float period
      let n3 =
        let hh = Stat.max period data
        let ll = Stat.min period data
        Array.map2 (fun h l -> (h - l) / n) hh ll

      let halfPeriod = period / 2
      let n = float halfPeriod
      let n1, n2 =
        let hh = Stat.max halfPeriod data
        let ll = Stat.min halfPeriod data
        let n12 = Array.map2 (fun h l -> (h - l) / n) hh ll
        n12.[0..n12.Length-1 - halfPeriod], n12.[halfPeriod..]

      let mutable prevOut = data.[startIdx]
      let mutable dimen = 0.0
      let hpOffset = n1.Length - n3.Length
      let log2 = log 2.0
      for i in 0 .. Array.length n3 - 1 do
        let hpIdx = i + hpOffset
        if n1.[hpIdx] > 0.0 && n2.[hpIdx] > 0.0 && n3.[i] > 0.0 then
          dimen <- (log(n1.[hpIdx] + n2.[hpIdx]) - log(n3.[i])) / log2
        let alpha =
          exp (-4.60 * (dimen - 1.0)) |> max 0.01 |> min 1.0

        out.[i] <- alpha*data.[i+lookbackTotal] + (1.0-alpha) * prevOut
        prevOut <- out.[i]

      out
    )
