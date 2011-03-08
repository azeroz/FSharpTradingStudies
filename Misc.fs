namespace Trading.Studies

module Misc =

  let slopeLookback (period:int) = period

  (* (value_n - value_0) / numberOfPeriods *)
  [<TradingStudy;
    Group("Misc");
    Title("Slope");
    Description("Returns the unit slope between n observations");
  >]
  let slope period (data:float[]) =
    Study.checkPositiveIntMin1 period
    let lookbackTotal = slopeLookback period
    let startIdx = lookbackTotal
    let endIdx = data.Length - 1
    Study.lazyCompute startIdx endIdx (fun outLen ->
      let n = float period
      Array.init (data.Length - period) (fun i -> (data.[i + period] - data.[i]) / n)
    )

  let wilSumLookback period = period - 1

  [<TradingStudy;
    Group("Misc");
    Title("Wilder summation");
    Description("Returns the Wilder summation");
  >]
  let wilSum period (data:float[]) =
    Study.checkPositiveIntMin1 period
    let lookbackTotal = wilSumLookback period
    let startIdx = lookbackTotal
    let endIdx = data.Length - 1
    Study.lazyCompute startIdx endIdx (fun outLen ->
      let out = Array.zeroCreate outLen

      let coeff = 1.0 - (1.0 / float period)

      for i in startIdx - lookbackTotal .. startIdx do
        out.[0] <- out.[0] + data.[i]

      for i in startIdx + 1 .. endIdx do
        let outIdx = i - startIdx
        out.[outIdx] <- coeff * out.[outIdx-1] + data.[i]
      out
    )

  //==========================================
  //
  // NOT CHECKED
  //
  //==========================================  

  let cftppLookback = 1

  ///Chicago Floor Trading Pivotal Point

  [<TradingStudy;
    Group("Misc");
    Title("Chicago Floor Trading Pivotal Point");
    Description("Returns the Chicago floor trading pivotal point");
    OutputSeriesNames("s2, s1, r1, r2");
  >]
  let cftpp (h:float[]) l c =
    Study.checkHighLowClose h l c
    let lookbackTotal = cftppLookback
    let startIdx = lookbackTotal
    let endIdx = h.Length - 1
    Study.lazyCompute4 startIdx endIdx (fun outLen ->
      let typ = Price.typ h l c
      ( Array.init outLen (fun i -> typ.[i+1] - h.[i] + l.[i]),
        Array.init outLen (fun i -> 2.0 * typ.[i+1] - h.[i]),
        Array.init outLen (fun i -> 2.0 * typ.[i+1] - l.[i]),
        Array.init outLen (fun i -> typ.[i+1] + h.[i] - l.[i])
      )
    )

  let pvrLookback (period:int) = period

  [<TradingStudy;
    Group("Volume");
    Title("Price Volume Rank");
    Description("Returns the price volume rank");
  >]
  let pvr period (data:float[]) volume =
    Study.checkPositiveIntMin1 period
    Study.checkSameInputLength [volume; data]
    Study.checkVolume volume
    let lookbackTotal = pvrLookback period
    let startIdx = lookbackTotal
    let endIdx = data.Length - 1
    Study.lazyCompute startIdx endIdx (fun outLen ->
      //1 : price and volume increase
      //2 : price increase without volume
      //3 : price decrease without volume
      //4 : price decrease with volume
      Array.init outLen (fun prev ->
        let today = prev + period
        if data.[today] > data.[prev] then
          if volume.[today] > volume.[prev] then 1.0 else 2.0
        elif
          volume.[today] > volume.[prev] then 4.0
        else
          3.0
      )
    )

  ///Ease of movement
  let eomLookback = 1

  [<TradingStudy;
    Group("Volume");
    Title("Ease of movement");
    Description("Returns the ease of movement indicator");
  >]
  let eom volumeDivisor h l volume =
    Study.checkPositiveReal volumeDivisor
    Study.checkSameInputLength [volume; h]
    Study.checkHighLow h l
    Study.checkVolume volume
    let lookbackTotal = eomLookback
    let startIdx = lookbackTotal
    let endIdx = h.Length - 1
    Study.lazyCompute startIdx endIdx (fun outLen ->
      Array.init outLen (fun prev ->
        let today = prev + 1
        let delta = h.[today] - l.[today]
        if delta <> 0.0 && volume.[today] <> 0.0 then
          (0.5 * (h.[today] - l.[today] - h.[prev] + l.[prev])) / ((volume.[prev] / volumeDivisor) / delta)
        else
          0.0
      )
    )

  ///Trend score
  let tsLookback period =
    Math.sumLookback period + 1

  [<TradingStudy;
    Group("Misc");
    Title("Trend score");
    Description("Returns the ease of movement indicator");
  >]
  let ts period (data:float[]) =
    Study.checkPositiveIntMin1 period
    let lookbackTotal = tsLookback period
    let startIdx = lookbackTotal
    let endIdx = data.Length - 1
    Study.lazyCompute startIdx endIdx (fun outLen ->
      Array.init outLen (fun prev ->
        let today = prev + 1
        if data.[today] >= data.[prev] then 1.0 else (-1.0)
      ) |> Math.sum period
    )

  (*  Tushar S. Chande - March, 1992 - Technical Analysis of Stocks & Commodities magazine
      A standard deviation was used as the Volatility Index. 

      In his October, 1995 article in the same magazine, Chande modified the VIDYA to use
      his own Chande momentum Osc (CMO) as the Volatility Index. 

      Examples of volatility indexes :
      - standard deviation (e.g., 9 period)
      - standard deviation percentage oscillator (e.g., 10 period divd by 50 period)
      - Chande momentum Osc (e.g., 9 period)
  *)

  let vidyaLookback period =
    Osc.cmoLookback period - 1

  [<TradingStudy;
    Group("Smoothing");
    Title("Vidya MA");
    Description("Returns the Vidya (or Variable) moving average");
  >]
  let vidya period (data:float[]) =
    Study.checkPositiveIntMin1 period
    let lookbackTotal = tsLookback period
    let startIdx = lookbackTotal
    let endIdx = data.Length - 1
    Study.lazyCompute startIdx endIdx (fun outLen ->
      let out = Array.zeroCreate outLen

      let n = float period
      let coeff = 2.0 / (n + 1.0)

      let cmo = Osc.cmo period data

      for i in startIdx - lookbackTotal .. startIdx do
        out.[0] <- out.[0] + (data.[i] / n)

      for i in startIdx + 1 .. endIdx do
        let outIdx = i - startIdx
        let cmoIdx = outIdx
        let alpha = coeff * cmo.[cmoIdx]
        out.[outIdx] <- alpha * data.[i] + (1.0 - alpha) * out.[outIdx-1]

      out
    )
