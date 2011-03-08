namespace Trading.Studies

module Osc =

  let wmomLookback period = BaseStudies.wmomLookback period

  [<TradingStudy;
    Group("Cycle");
    Title("Weighted Momentum");
    Description("Returns the weighted momentum");
  >]
  let wmom coeffToday coeffLag period data =
    BaseStudies.wmom coeffToday coeffLag period data

  let momLookback period = BaseStudies.momLookback period
  [<TradingStudy;
    Group("Cycle");
    Title("Momentum");
    Description("Returns the momentum");
  >]
  let mom period data = BaseStudies.mom period data

  let rocLookback period = BaseStudies.rocLookback period
  [<TradingStudy;
    Group("Cycle");
    Title("Rate of Change");
    Description("Returns the rate of change");
  >]
  let roc isBase100 divByZeroDefaultValue period data =
    BaseStudies.roc isBase100 divByZeroDefaultValue period data

  (*  The Relative momentum Index is a variation on the Relative Strength Index.
      To determine up and down days, the RSI uses the close compared to the previous close.
      The RMI uses the close compared to the close n days ago.
      An RMI with a time period of 1 is equal to the RSI. 

      The Relative momentum Index was developed by Roger Altman and was introduced
      in his article in the February, 1993 issue of Technical Analysis of Stocks & Commodities magazine.

      Note that since RMI is more "versatile" than RSI, we define the RSI as a function of the RMI.

      See the RSI module for more information.

      RMI = 100 - (100 / (1 + RM)) where RM = average gain / average loss over the period
      <=> RMI = 100 * (avg_gain / (avg_gain+avg_loss))
      average gain = moving average of the up moves
      average loss = moving average of the down moves
      where the moving averages have a period = average_period
      and up_move_i and down_move_i are based upon value_i - value_(i-momentum_period)

      Note that since gains (resp. losses) are smoothed, there is a convergence effect :
      the first values of the RMI are less smoothed than the following ones.

      Therefore, the further one advances in the calculation, the less impact this initial
      divergence has, the more stability you have.
  *)

  let gainsLookback period = period - 1

  [<TradingStudy;
    Group("Misc");
    Title("Gains");
    Description("Returns the n-period gains");
  >]
  let gains period (data:float[]) =
    let lookbackTotal = gainsLookback period
    let startIdx = lookbackTotal
    let endIdx = data.Length - 1
    Study.lazyCompute startIdx endIdx (fun outLen ->
      data
        |> mom period
        |> Array.map (max 0.0)
    )

  let lossesLookback period = period - 1
  [<TradingStudy;
    Group("Misc");
    Title("Losses");
    Description("Returns the n-period losses");
  >]
  let losses period (data:float[]) =
    let lookbackTotal = lossesLookback period
    let startIdx = lookbackTotal
    let endIdx = data.Length - 1
    Study.lazyCompute startIdx endIdx (fun outLen ->
      data
        |> mom period
        |> Array.map (min 0.0)
    )

  let rmiLookback momPeriods avgPeriods =
    momLookback momPeriods + MA.wilmaLookback avgPeriods

  let rmiAux gain loss =
    if gain <> loss then 100.0 * (gain / (gain - loss)) else 0.0  

  [<TradingStudy;
    Group("Cycle");
    Title("Relative Momentum Index");
    Description("Returns the relative momentum index");
  >]
  let rmi momPeriods avgPeriods (data:float[]) =
    let lookbackTotal = rmiLookback momPeriods avgPeriods
    let startIdx = lookbackTotal
    let endIdx = data.Length - 1
    Study.lazyCompute startIdx endIdx (fun outLen ->
      let gains =
        data
          |> gains momPeriods
          |> MA.wilma avgPeriods
      let losses =
        data
          |> losses momPeriods
          |> MA.wilma avgPeriods
      Array.map2 (fun gain loss -> rmiAux gain loss) gains losses
    )

  let rsiLookback period = rmiLookback 1 period

  [<TradingStudy;
    Group("Cycle");
    Title("Relative Strength Index");
    Description("Returns the relative strength index");
  >]
  let rsi period data =
    rmi 1 period data

  //Tuschar Chande momentum oscillator
  let cmoLookback avgPeriods =
    momLookback 1 + MA.wilmaLookback avgPeriods

  let cmoAux gain loss =
    if gain <> loss then
      100.0 * (gain + loss) / (gain - loss)
    else
      0.0

  [<TradingStudy;
    Group("Cycle");
    Title("Chande Momentum Oscillator");
    Description("Returns the Chande momentum oscillator");
  >]
  let cmo avgPeriods (data:float[]) =
    let lookbackTotal = cmoLookback avgPeriods
    let startIdx = lookbackTotal
    let endIdx = data.Length - 1
    Study.lazyCompute startIdx endIdx (fun outLen ->
      let gains =
        data
          |> gains 1
          |> MA.wilma avgPeriods
      let losses =
        data
          |> losses 1
          |> MA.wilma avgPeriods
      Array.map2 (fun gain loss -> cmoAux gain loss) gains losses
    )

  // Donald R. Lambert - CCI
  let cciLookback period = MA.smaLookback period

  [<TradingStudy;
    Group("Cycle");
    Title("Commodity Channel Index");
    Description("Returns the cmmodity channel index");
  >]
  let cci period (h:float[]) l c =
    let lookbackTotal = cciLookback period
    let startIdx = lookbackTotal
    let endIdx = h.Length - 1
    Study.lazyCompute startIdx endIdx (fun outLen ->
      let typ = Price.typ h l c
      let ma = MA.sma period typ
      let absdev = Stat.avgDev period typ
      let offset = typ.Length - ma.Length
      Array.init ma.Length (fun i ->
        if absdev.[i] <> 0.0 then (typ.[i + offset] - ma.[i]) / (0.015 * absdev.[i]) else 0.0
      )
    )

  let trixLookback period =
    3 * MA.emaLookback period + rocLookback 1

  [<TradingStudy;
    Group("Cycle");
    Title("Trix");
    Description("Returns the Trix");
  >]
  let trix period (data:float[]) =
    let lookbackTotal = trixLookback period
    let startIdx = lookbackTotal
    let endIdx = data.Length - 1
    Study.lazyCompute startIdx endIdx (fun outLen ->
      data
        |> MA.ema period
        |> MA.ema period
        |> MA.ema period
        |> roc true 100.0 1
    )

  ///Williams %R
  let willRLookback period = period - 1

  let willRAux h l c =
    let delta = h - l
    if delta <> 0.0 then (-100.0) * (h - c) / delta else 0.0

  [<TradingStudy;
    Group("Cycle");
    Title("Williams %R");
    Description("Returns the Williams %R");
  >]
  let willR period h l c =
    Study.checkPositiveIntMin1 period
    Study.checkHighLowClose h l c
    let lookbackTotal = willRLookback period
    let startIdx = lookbackTotal
    let endIdx = h.Length - 1
    Study.lazyCompute startIdx endIdx (fun outLen ->
      let hh = Stat.max period h
      let ll = Stat.max period l
      let offset = h.Length - hh.Length
      Array.init outLen (fun i -> willRAux h.[i] l.[i] c.[i+offset])
    )

  ///Stochastics Fast K
  let stochFastKLookback period = Stat.maxLookback period

  let stochFastKAux hh ll c =
    let delta = hh - ll
    if delta <> 0.0 then 100.0 * (c - ll) / delta else 0.0

  [<TradingStudy;
    Group("Cycle");
    Title("Stochastics Fast K");
    Description("Returns the stochastics fast K");
  >]
  let stochFastK period h l c =
    Study.checkPositiveIntMin1 period
    Study.checkHighLowClose h l c
    let lookbackTotal = stochFastKLookback period
    let startIdx = lookbackTotal
    let endIdx = h.Length - 1
    Study.lazyCompute startIdx endIdx (fun outLen ->
      let hh = Stat.max period h
      let ll = Stat.min period l
      let offset = h.Length - hh.Length
      Array.init outLen (fun i -> stochFastKAux hh.[i] ll.[i] c.[i+offset])
    )

  ///Stochastics Fast (Fast K + Slow K)
  let stochFastLookback fastKPeriod (ma:MA) slowKPeriod =
    stochFastKLookback fastKPeriod + ma.lookback slowKPeriod

  [<TradingStudy;
    Group("Cycle");
    Title("Stochastics Fast K/Slow K");
    Description("Returns the stochastics fast K and slow K");
    OutputSeriesNames("fast K, slow K");
  >]
  let stochFast fastKPeriod (ma:MA) slowKPeriod (h:float[]) l c =
    let lookbackTotal = stochFastLookback fastKPeriod ma slowKPeriod
    let startIdx = lookbackTotal
    let endIdx = h.Length - 1
    Study.lazyCompute2 startIdx endIdx (fun outLen ->
      let fastK = stochFastK fastKPeriod h l c
      let slowK = ma.create slowKPeriod fastK
      let offset = fastK.Length - slowK.Length
      fastK.[offset..], slowK
    )

  ///Stochastics Slow (Fast D + Slow D, where Fast D = Slow K)
  let stochSlowLookback fastKPeriod maK slowKPeriod (maD:MA) slowDPeriod =
    stochFastLookback fastKPeriod maK slowKPeriod + maD.lookback slowDPeriod

  [<TradingStudy;
    Group("Cycle");
    Title("Stochastics Slow K/Slow D");
    Description("Returns the stochastics slow K and slow D");
    OutputSeriesNames("slow K, slow D");
  >]
  let stochSlow fastKPeriod maK slowKPeriod maD slowDPeriod (h:float[]) l c =
    let lookbackTotal = stochSlowLookback fastKPeriod maK slowKPeriod maD slowDPeriod
    let startIdx = lookbackTotal
    let endIdx = h.Length - 1
    Study.lazyCompute2 startIdx endIdx (fun outLen ->
      let fastK, slowK = stochFast fastKPeriod maK slowKPeriod h l c
      let slowD = maD.create slowDPeriod slowK
      let offset = slowK.Length - slowD.Length
      slowK.[offset..], slowD
    )

  ///Moving Average Convergence Divergence
  let macdLookback (ma:MA) slowPeriod signalPeriod =
    ma.lookback slowPeriod + ma.lookback signalPeriod

  [<TradingStudy;
    Group("Cycle");
    Title("MACD");
    Description("Returns the moving average convergence-divergence");
    OutputSeriesNames("macd, signal, histogram");
  >]
  let macd (ma:MA) fastPeriod slowPeriod signalPeriod (data:float[]) =
    if fastPeriod > slowPeriod then
      invalidArg "fastPeriod" "fastPeriod > slowPeriod"
    let lookbackTotal = macdLookback ma slowPeriod signalPeriod
    let startIdx = lookbackTotal
    let endIdx = data.Length - 1
    Study.lazyCompute3 startIdx endIdx (fun outLen ->
      let fastMA = ma.create fastPeriod data
      let slowMA = ma.create slowPeriod data
      let offset = fastMA.Length - slowMA.Length
      let macd = Array.init slowMA.Length (fun i -> fastMA.[i + offset] - slowMA.[i])
      let signal = ma.create signalPeriod macd
      let offset = macd.Length - signal.Length
      let hist = Array.init signal.Length (fun i -> macd.[i + offset] - signal.[i])
      macd.[offset..], signal, hist
    )

  ///Detrended price oscillator
  let dpoPeriod period = period / 2 + 1

  let dpoLookback period =
    MA.smaLookback (dpoPeriod period)

  [<TradingStudy;
    Group("Cycle");
    Title("Detrended Price Oscillator");
    Description("Returns the detrended price oscillator");
  >]
  let dpo period data =
    let ma = MA.sma (dpoPeriod period) data
    let offset = data.Length - ma.Length
    Array.init ma.Length (fun i -> data.[i + offset] - ma.[i])

  ///Chande Dynamic Momentum Index
  ///Tushar S. Chande and Stanley Kroll and is described in their 1994 book The New Technical Trader
  let cdmiLookback =
    Stat.stDevLookback 5 + MA.smaLookback 10

  [<TradingStudy;
    Group("Cycle");
    Title("Dynamic Momentum Index");
    Description("Returns the Chande dynamic momentum index");
  >]
  let cdmi data =
    let sd = Stat.stDev true 5 data
    let asd = MA.sma 10 sd
    let offset = sd.Length - asd.Length
    Array.init asd.Length (fun i -> 14.0 * asd.[i] / sd.[i + offset])

  ///On-Balance Volume
  let obvLookback = 0

  [<TradingStudy;
    Group("Volume");
    Title("On-Balance Volume");
    Description("Returns the on-balance volume");
  >]
  let obv (data:float[]) volume =
    Study.checkSameInputLength [volume; data]
    Study.checkVolume volume
    let lookbackTotal = obvLookback
    let startIdx = lookbackTotal
    let endIdx = data.Length - 1
    Study.lazyCompute startIdx endIdx (fun outLen ->
      let out = Array.zeroCreate outLen

      out.[0] <- volume.[startIdx]

      for today in startIdx + 1 .. endIdx do
        let outIdx = today - startIdx
        if data.[today] > data.[today - 1] then
          out.[outIdx] <- out.[outIdx-1] + volume.[today]
        elif data.[today] < data.[today - 1] then
          out.[outIdx] <- out.[outIdx-1] - volume.[today]
        else
          out.[outIdx] <- out.[outIdx-1]

      out
    )

  ///Accumulation / Distribution
  let adLookback = 0

  //CLV = ((C - L) - (H - C)) / (H - L)
  //    = (2C - H - L) / (H-L)
  let volumeClv h l c v =
    let delta = h - l
    if delta <> 0.0 then (2.0 * c - h - l) / delta * v else 0.0

  let adLineClv h l c volume =
    Study.checkSameInputLength [volume; h]
    Study.checkVolume volume
    Study.checkHighLowClose h l c
    let lookbackTotal = adLookback
    let startIdx = lookbackTotal
    let endIdx = h.Length - 1
    Study.lazyCompute startIdx endIdx (fun outLen ->
      Array.init outLen (fun i -> volumeClv h.[i] l.[i] c.[i] volume.[i])
    )  

  [<TradingStudy;
    Group("Volume");
    Title("Accumulation / Distribution");
    Description("Returns the Chande accumulation / distribution");
  >]
  let ad h l c volume =
    let adLine = adLineClv h l c volume
    let startIdx = adLookback
    let endIdx = h.Length - 1
    Study.lazyCompute startIdx endIdx (fun outLen ->
      let out = Array.zeroCreate outLen
      out.[0] <- adLine.[0]
      for today in startIdx + 1.. endIdx do
        let outIdx = today - startIdx
        out.[outIdx] <- out.[outIdx - 1] + adLine.[today]

      out
    )

  ///Chaikin Osc
  let chOscLookback slowPeriod =
    adLookback + MA.emaLookback slowPeriod

  [<TradingStudy;
    Group("Cycle");
    Title("Chaikin Oscillator");
    Description("Returns the Chaikin oscillator");
  >]
  let chOsc fastPeriod slowPeriod (h:float[]) l c volume =
    if fastPeriod > slowPeriod then
      raise <| BadParam "fast period > slowPeriod"
    let lookbackTotal = chOscLookback slowPeriod
    let startIdx = lookbackTotal
    let endIdx = h.Length - 1
    Study.lazyCompute startIdx endIdx (fun outLen ->
      let ad = ad h l c volume
      let fast = MA.ema fastPeriod ad
      let slow = MA.ema slowPeriod ad
      let offset = fast.Length - slow.Length
      Array.init slow.Length (fun i -> fast.[i + offset] - slow.[i])
    )  

  ///Chaikin Money Flow
  let chMfLookback period =
    adLookback + Math.sumLookback period

  [<TradingStudy;
    Group("Cycle");
    Title("Chaikin Money Flow");
    Description("Returns the Chaikin money flow");
  >]
  let chMf period (h:float[]) l c volume =
    Study.checkPositiveIntMin1 period
    let lookbackTotal = chMfLookback period
    let startIdx = lookbackTotal
    let endIdx = h.Length - 1
    Study.lazyCompute startIdx endIdx (fun outLen ->
      //adLineClv has 0 lookback, so we can use a map2 afterward
      let ad = adLineClv h l c volume |> Math.sum period
      let vol = Math.sum period volume
      Array.map2 ( / ) ad vol
    )

  ///Balance of Power
  let bpowLookback = 0

  [<TradingStudy;
    Group("Cycle");
    Title("Balance of Power");
    Description("Returns the balance of power");
  >]
  let bpow o h l c =
    Study.checkOpenHighLowClose o h l c
    let lookbackTotal = bpowLookback
    let startIdx = lookbackTotal
    let endIdx = h.Length - 1
    Study.lazyCompute startIdx endIdx (fun outLen ->
      Array.init outLen (fun i ->
        let idx = startIdx + i
        let delta = h.[idx] - l.[idx]
        if delta <> 0.0 then (c.[idx] - o.[idx]) / delta else 0.0
      )
    )  

  let aroonLookback period = Stat.maxLookback period + 1

  [<TradingStudy;
    Group("Cycle");
    Title("Aroon");
    Description("Returns the Aroon indicator");
    OutputSeriesNames("aroon down, aroon up");
  >]
  let aroon period h l =
    Study.checkPositiveIntMin1 period
    Study.checkHighLow h l
    let lookbackTotal = aroonLookback period
    let startIdx = lookbackTotal
    let endIdx = h.Length - 1
    Study.lazyCompute2 startIdx endIdx (fun outLen ->
      let extremasPeriod = period + 1
      let highestHighs = Stat.maxIdx extremasPeriod h
      let lowestLows = Stat.minIdx extremasPeriod l
      let aroonUp = Array.zeroCreate outLen
      let aroonDown = Array.zeroCreate outLen
      let n = float period
      let coeff = 100.0 / n
      let f pos extremaIdx = coeff * (n - float (pos - extremaIdx))

      Array.iteri2 (fun i hh ll ->
        let g = f (startIdx + i)
        aroonUp.[i] <- g hh
        aroonDown.[i] <- g ll
      ) highestHighs lowestLows

      aroonDown, aroonUp
    )  

  let aroonOscLookback period = aroonLookback period

  [<TradingStudy;
    Group("Cycle");
    Title("Aroon oscillator");
    Description("Returns the Aroon oscillator");
  >]
  let aroonOsc period h l =
    let down, up = aroon period h l
    Array.map2 (-) up down

  ///Money Flow Index
  let mfiLookback period =
    Price.typLookback + Math.sumLookback period

  let mfiAux plus minus =
    //100 - (100 / (1 + MoneyRatio)) <=> 100 * sum+ / (sum+ + sum-)
    //to avoid excessively large values, we ilter based on the denominator
    let mr = plus / minus
    if mr = 1.0 then 0.0 else 100.0 * (mr / (1.0 + mr))

  [<TradingStudy;
    Group("Cycle");
    Title("Money Flow Index");
    Description("Returns the money flow index");
  >]
  let mfi period h l c volume =
    Study.checkPositiveIntMin1 period
    Study.checkVolume volume
    Study.checkHighLowClose h l c
    let lookbackTotal = aroonLookback period
    let startIdx = lookbackTotal
    let endIdx = h.Length - 1
    Study.lazyCompute startIdx endIdx (fun outLen ->
      let tp = Price.typ h l c
      let mf = Array.map2 ( * ) tp volume
      let mfPlus = Array.zeroCreate tp.Length
      let mfMinus = Array.zeroCreate tp.Length
      for i in 1 .. tp.Length - 1 do
        if tp.[i] > tp.[i - 1] then
          mfPlus.[i] <- mfPlus.[i-1] + mf.[i]
        else
          mfMinus.[i] <- mfMinus.[i-1] + mf.[i]
      Array.map2 mfiAux (Math.sum period mfPlus) (Math.sum period mfMinus)
    )  

  ///Absolute Price Osc
  let apoLookback (ma:MA) slowPeriod =
    ma.lookback slowPeriod

  [<TradingStudy;
    Group("Cycle");
    Title("Absolute Price Oscillator");
    Description("Returns the absolute price oscillator");
  >]
  let apo (ma:MA) fastPeriod slowPeriod (data:float[]) =
    Study.checkPositiveIntMin1 fastPeriod
    if fastPeriod > slowPeriod then
      invalidArg "fastPeriod" "fast period > slow period"
    let lookbackTotal = apoLookback ma slowPeriod
    let startIdx = lookbackTotal
    let endIdx = data.Length - 1
    Study.lazyCompute startIdx endIdx (fun outLen ->
      let fastMA = ma.create fastPeriod data
      let slowMA = ma.create slowPeriod data
      let offset = fastMA.Length - slowMA.Length
      Array.init slowMA.Length (fun i -> fastMA.[i + offset] - slowMA.[i])
    )

  ///Percentage Price Osc
  let ppoLookback (ma:MA) slowPeriod =
    ma.lookback slowPeriod

  [<TradingStudy;
    Group("Cycle");
    Title("Percentage Price Oscillator");
    Description("Returns the percentage price oscillator");
  >]
  let ppo (ma:MA) fastPeriod slowPeriod (data:float[]) =
    Study.checkPositiveIntMin1 fastPeriod
    if fastPeriod > slowPeriod then
      invalidArg "fastPeriod" "fast period > slow period"
    let lookbackTotal = apoLookback ma slowPeriod
    let startIdx = lookbackTotal
    let endIdx = data.Length - 1
    Study.lazyCompute startIdx endIdx (fun outLen ->
      let fastMA = ma.create fastPeriod data
      let slowMA = ma.create slowPeriod data
      let offset = fastMA.Length - slowMA.Length
      Array.init slowMA.Length (fun i -> 100.0 * (fastMA.[i + offset] / slowMA.[i] - 1.0))
    )

  ///Vertical Horizon Filter
  let vhfLookback period =
    Math.sumLookback period + rocLookback 1

  [<TradingStudy;
    Group("Cycle");
    Title("Vertical Horizon Filter");
    Description("Returns the vertical horizon filter");
  >]
  let vhf period (h:float[]) l c =
    Study.checkPositiveIntMin1 period
    Study.checkHighLowClose h l c
    let lookbackTotal = vhfLookback period
    let startIdx = lookbackTotal
    let endIdx = h.Length - 1
    Study.lazyCompute startIdx endIdx (fun outLen ->
      let highs = Stat.max period h
      let lows = Stat.min period l
      let rocSum = roc true 0.0 1 c |> Math.sum period
      let offset = highs.Length - rocSum.Length
      Array.init rocSum.Length (fun i ->
        (highs.[i+offset] - lows.[i+offset]) / rocSum.[i]
      )
    )

