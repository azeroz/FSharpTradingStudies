namespace Trading.Studies

type MA =
  | Simple
  | Wilder
  | Exponential
  | DoubleExponential
  | TripleExponential
  | Tillson3 of float
  | Triangular
  | Weighted
  | KaufmanAdaptive
  | ZeroLag of float
  | VolumeWeighted of float[]
  | Hull
  | SineWeighted 

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module MA =

  let smaLookback period = Stat.meanLookback period

  [<TradingStudy;
    Group("Smoothing");
    Title("Simple MA");
    Description("Returns the simple moving average");
    Overlay;
  >]
  let sma period data = Stat.mean period data

  let baseEmaLookback period = period - 1

  let baseEMA period coeff (data:float[]) =
    Study.checkPositiveIntMin1 period
    let lookbackTotal = baseEmaLookback period
    let startIdx = lookbackTotal
    let endIdx = data.Length - 1
    Study.lazyCompute startIdx endIdx (fun outLen ->
      let out = Array.zeroCreate outLen

      let n = float period

      for i in startIdx - lookbackTotal .. startIdx do
        out.[0] <- out.[0] + (data.[i] / n)

      for i in startIdx+1 .. endIdx do
        let outIdx = i - startIdx
        let prev = out.[outIdx-1]
        out.[outIdx] <- prev + coeff * (data.[i] - prev)

      out
    )

  let wilmaLookback period = baseEmaLookback period

  [<TradingStudy;
    Group("Smoothing");
    Title("Wilder MA");
    Description("Returns the Wilder moving average");
    Overlay;
  >]
  let wilma period data =
    let coeff = 1.0 / float period
    baseEMA period coeff data

  let emaLookback period = baseEmaLookback period

  [<TradingStudy;
    Group("Smoothing");
    Title("Exponential MA");
    Description("Returns the exponential moving average");
    Overlay;
  >]
  let ema period data =
    let coeff = 2.0 / (1.0 + float period)
    baseEMA period coeff data    

  let demaLookback period = 2 * (emaLookback period)

  [<TradingStudy;
    Group("Smoothing");
    Title("Double Exponential MA");
    Description("Returns the double exponential moving average");
    Overlay;
  >]
  let dema period (data:float[]) =
    let lookbackTotal = baseEmaLookback period
    let startIdx = demaLookback period
    let endIdx = data.Length - 1
    Study.lazyCompute startIdx endIdx (fun outLen ->
      let ema1 = ema period data
      let ema2 = ema period ema1
      let offset = ema1.Length - ema2.Length
      Array.init ema2.Length (fun i -> 2.0 * ema1.[i + offset] - ema2.[i])
    ) 

  let temaLookback period = 3 * (emaLookback period)

  [<TradingStudy;
    Group("Smoothing");
    Title("Triple Exponential MA");
    Description("Returns the triple exponential moving average");
    Overlay;
  >]
  let tema period (data:float[]) =
    let lookbackTotal = baseEmaLookback period
    let startIdx = temaLookback period
    let endIdx = data.Length - 1
    Study.lazyCompute startIdx endIdx (fun outLen ->
      let ema1 = ema period data
      let ema2 = ema period ema1
      let ema3 = ema period ema2
      let o13 = ema1.Length - ema3.Length
      let o23 = ema2.Length - ema3.Length
      Array.init ema3.Length (fun i ->
        3.0 * ema1.[i + o13] - 3.0 * ema2.[i + o23] + ema3.[i]
      )
    )

  (* Tim Tillson - TechnicalAnalysis of Stocks and Commodities - January 1998 *)
  let t3Lookback period = 6 * (emaLookback period)

  [<TradingStudy;
    Group("Smoothing");
    Title("Tillson's T3");
    Description("Returns Tillson's T3 - TechnicalAnalysis of Stocks and Commodities (Jan. 1998)");
    Overlay;
  >]
  let t3 period volumeFactor (data:float[]) =
    let lookbackTotal = baseEmaLookback period
    let startIdx = t3Lookback period
    let endIdx = data.Length - 1
    Study.lazyCompute startIdx endIdx (fun outLen ->
      let ema1 = ema period data
      let ema2 = ema period ema1
      let ema3 = ema period ema2
      let ema4 = ema period ema3
      let ema5 = ema period ema4
      let ema6 = ema period ema5
      //constants
      let vf2 = volumeFactor * volumeFactor
      let vf3 = vf2 * volumeFactor
      let c1 = -vf3
      let c2 = 3.0 * (vf2 + vf3)
      let c3 = -6.0*vf2 - 3.0*(volumeFactor + vf3)
      let c4 = 1.0 + 3.0*volumeFactor + vf3 + 3.0*vf2
      //offsets
      let lookbackOneEma = emaLookback period
      let o36 = 3 * lookbackOneEma
      let o46 = 2 * lookbackOneEma
      let o56 = lookbackOneEma
      //output
      Array.init ema6.Length (fun i ->
        c1*ema6.[i] + c2*ema5.[i+o56] + c3*ema4.[i+o46] + c4*ema3.[i+o36]
      )
    )

  (* A triangular SMA is an SMA of SMA whose period are computed as follow :
      if period = even -> SMA(period/2 + 1, SMA(period/2, data))
      if period = odd -> SMA((period+1)/2, SMA((period+1)/2 , data))
  *)
  let trimaLookback period = smaLookback period

  [<TradingStudy;
    Group("Smoothing");
    Title("Triangular MA");
    Description("Returns the triangular moving average");
    Overlay;
  >]
  let trima period (data:float[]) =
    Study.checkPositiveIntMin1 period
    let lookbackTotal = trimaLookback period
    let startIdx = lookbackTotal
    let endIdx = data.Length - 1
    Study.lazyCompute startIdx endIdx (fun outLen ->
      let period1, period2 =
        if period % 2 = 0 then
          let tmp = period / 2
          tmp, tmp + 1
        else
          let tmp = (period+1) / 2
          tmp, tmp  

      sma period1 data |> sma period2
    )

  let wmaLookback period = period - 1

  [<TradingStudy;
    Group("Smoothing");
    Title("Weighted MA");
    Description("Returns the weighted moving average");
    Overlay;
  >]
  let wma period (data:float[]) =
    Study.checkPositiveIntMin1 period
    let lookbackTotal = wmaLookback period
    let startIdx = lookbackTotal
    let endIdx = data.Length - 1
    Study.lazyCompute startIdx endIdx (fun outLen ->
      let out = Array.zeroCreate outLen

      let n = float period
      let coeff = n * (n + 1.0) / 2.0
      let mutable substract = 0.0
      let mutable acc = 0.0

      let mutable weight = 1.0
      for i in startIdx - lookbackTotal .. startIdx do
        acc <- acc + data.[i] * weight
        weight <- weight + 1.0
        substract <- substract + data.[i]

      out.[0] <- acc / coeff

      for i in startIdx + 1 .. endIdx do
        let outIdx = i - startIdx
        acc <- acc + (n * data.[i]) - substract
        substract <- substract + data.[i] - data.[i-period]
        out.[outIdx] <- acc / coeff

      out
    )

  (*  Perry Kaufman - Stocks & Commodities magazine - March, 1995
      Note that contrary to most moving averages, even if period = 1, the output
      differs from the input. This is because "period" doesn't refer to the number
      of values serving as an input to construct an output, but it corresponds
      to an index offset, in the same way as it does for the Rate of change or momentum
      indicators
  *)
  let kamaLookback period =
    Math.sumLookback period + BaseStudies.momLookback 1

  [<TradingStudy;
    Group("Smoothing");
    Title("Kaufman Adaptive MA");
    Description("Returns the Kaufman adaptive moving average");
    Overlay;
  >]
  let kama period (data:float[]) =
    Study.checkPositiveIntMin1 period
    let lookbackTotal = kamaLookback period
    let startIdx = lookbackTotal
    let endIdx = data.Length - 1
    Study.lazyCompute startIdx endIdx (fun outLen ->
      let out = Array.zeroCreate outLen

      let direction = BaseStudies.mom period data

      let volatility =
        BaseStudies.mom 1 data |> Array.map abs |> Math.sum period

      let slow = 2.0 / 31.0
      let diff = (2.0/3.0) - slow

      let smoothAux i currentVolatility =
        let efficiencyRatio =
          let dir = direction.[i]
          if currentVolatility <> 0.0 then abs <| dir / currentVolatility else 1.0
        (slow + diff * efficiencyRatio) ** 2.0

      let smooth = Array.mapi smoothAux volatility
     
      let mutable prev = data.[startIdx-1]
      out.[0] <- prev + smooth.[0] * (data.[startIdx] - prev)

      for today in startIdx + 1 .. endIdx do
        let outIdx = today - startIdx
        prev <- out.[outIdx - 1]
        out.[outIdx] <- prev + smooth.[outIdx] * (data.[today] - prev)

      out
    )

  //==========================================
  //
  // NOT CHECKED
  //
  //==========================================  

  let zlmaLag period = (period - 1) / 2

  let zlmaLookback period = emaLookback period + zlmaLag period

  [<TradingStudy;
    Group("Smoothing");
    Title("Zero-Lag MA");
    Description("Returns the zero-lag moving average");
    Overlay;
  >]
  let zlma k period data =
    data
      |> BaseStudies.wmom (1.0+k) k (zlmaLag period)
      |> ema period

  let vwmaLookback period = period - 1

  [<TradingStudy;
    Group("Smoothing");
    Title("Volume-Weighted MA");
    Description("Returns the volume-weighted moving average");
    Overlay;
  >]
  let vwma period volume (data:float[]) =
    Study.checkPositiveIntMin1 period
    Study.checkVolume volume
    let lookbackTotal = vwmaLookback period
    let startIdx = lookbackTotal
    let endIdx = data.Length - 1
    Study.lazyCompute startIdx endIdx (fun outLen ->
      let out = Array.zeroCreate outLen

      let mutable sum = 0.0
      let mutable sumProd = 0.0

      for i in startIdx - lookbackTotal .. startIdx do
        sum <- sum + volume.[i]
        sumProd <- sumProd + (data.[i] * volume.[i])

      out.[0] <- sumProd / sum

      let trailingStartIdx = startIdx - lookbackTotal
      for i in startIdx + 1 .. endIdx do
        let outIdx = i - startIdx
        let trailingIdx = i - (lookbackTotal+1)
        sum <- sum + volume.[i] - volume.[trailingIdx]
        sumProd <- sumProd + (data.[i] * volume.[i]) - (data.[trailingIdx] * volume.[trailingIdx])
        out.[outIdx] <- sumProd / sum

      out
    )

  let hmaSquareRoot = float >> sqrt >> int

  let hmaLookbackSquareRoot period =
    wmaLookback (hmaSquareRoot period)

  let hmaLookback period =
    wmaLookback period + hmaLookbackSquareRoot period

  [<TradingStudy;
    Group("Smoothing");
    Title("Hull MA");
    Description("Returns the Hull moving average");
    Overlay;
  >]
  let hma period (data:float[]) =
    Study.checkPositiveIntMin1 period
    let lookbackTotal = hmaLookback period
    let startIdx = lookbackTotal
    let endIdx = data.Length - 1
    Study.lazyCompute startIdx endIdx (fun outLen ->
      let wma1 = wma (period/2) data
      let wma2 = wma period data
      let offset = wma1.Length - wma2.Length
      let deltas = Array.init wma2.Length (fun i -> 2.0 * wma1.[i+offset] - wma2.[i])
      wma (hmaSquareRoot period) deltas
    )

  let swmaLookback period = period - 1

  let swmaWeights period =
    let n = float period
    Array.init period (fun i -> (float (i+1) * System.Math.PI) / n |> sin)

  [<TradingStudy;
    Group("Smoothing");
    Title("Sine-Weighted MA");
    Description("Returns the sine-weighted moving average");
    Overlay;
  >]
  let swma period (data:float[]) =
    Study.checkPositiveIntMin1 period
    let lookbackTotal = vwmaLookback period
    let startIdx = lookbackTotal
    let endIdx = data.Length - 1
    Study.lazyCompute startIdx endIdx (fun outLen ->
      let ws = swmaWeights period
      let denom = Array.reduce (+) ws
      Array.init outLen (fun i ->
        let mutable sum = 0.0
        let today = startIdx + i
        for weightIdx in 0 .. period - 1 do
          sum <- sum + ws.[weightIdx] * data.[today - weightIdx]
        sum / denom
      )
    )

type MA
  with
    member x.create p =
      match x with
      | Simple -> MA.sma p
      | Wilder -> MA.wilma p
      | Exponential -> MA.ema p
      | DoubleExponential -> MA.dema p
      | TripleExponential -> MA.trima p
      | Tillson3 volumeFactor -> MA.t3 p volumeFactor
      | Triangular -> MA.trima p
      | Weighted -> MA.wma p
      | KaufmanAdaptive -> MA.kama p
      | ZeroLag k -> MA.zlma k p
      | VolumeWeighted volumeData -> MA.vwma p volumeData
      | Hull -> MA.hma p
      | SineWeighted -> MA.swma p 

    member x.lookback p =
      match x with
      | Simple -> MA.smaLookback p
      | Wilder -> MA.wilmaLookback p
      | Exponential -> MA.emaLookback p
      | DoubleExponential -> MA.demaLookback p
      | TripleExponential -> MA.trimaLookback p
      | Tillson3 volumeFactor -> MA.t3Lookback p
      | Triangular -> MA.trimaLookback p
      | Weighted -> MA.wmaLookback p
      | KaufmanAdaptive -> MA.kamaLookback p
      | ZeroLag _ -> MA.zlmaLookback p
      | VolumeWeighted volumeData -> MA.vwmaLookback p
      | Hull -> MA.hmaLookback p
      | SineWeighted -> MA.swmaLookback p 
