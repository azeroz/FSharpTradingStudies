namespace Trading.Studies

open Trading.Studies
module Band =

  let bandsLookback = 0

  [<TradingStudy;
    Group("Bands");
    Title("Bands");
    Description("Returns an enveloppe built by scaling the input series");
    OutputSeriesNames("lower, upper");
    Overlay;
  >]
  let bands factor (data:float[]) =
    Study.checkPositiveReal factor
    let lookbackTotal = bandsLookback
    let startIdx = lookbackTotal
    let endIdx = data.Length - 1
    Study.lazyCompute2 startIdx endIdx (fun outLen ->
      Array.map (fun x -> x * (1.0 - factor)) data,
      Array.map (fun x -> x * (1.0 + factor)) data
    )

  //Bollinger bands
  let bbandsLookback (ma:MA) period =
    max (Stat.stDevLookback period) (ma.lookback period)

  [<TradingStudy;
    Group("Bands");
    Title("Bollinger Bands");
    Description("Returns the Bollinger bands");
    OutputSeriesNames("lower, middle, upper");
    Overlay;
  >]
  let bbands (ma:MA) period stdevUp stdevDown (data:float[]) =
    Study.checkPositiveIntMin1 period
    Study.checkPositiveReal stdevUp
    Study.checkPositiveReal stdevDown
    let lookbackTotal = bbandsLookback ma period
    let startIdx = lookbackTotal
    let endIdx = data.Length - 1
    Study.lazyCompute3 startIdx endIdx (fun outLen ->
      let mid = ma.create period data
      let stdev = Stat.stDev false period data
      let h = Array.map2 (fun mid stdev -> mid + stdevUp * stdev) mid stdev
      let l = Array.map2 (fun mid stdev -> mid - stdevDown * stdev) mid stdev
      l, mid, h
    )

  //Donchian channel
  let dchanLookback period =
    Stat.maxLookback period

  [<TradingStudy;
    Group("Bands");
    Title("Donchian Channel");
    Description("Returns the Donchian channel");
    OutputSeriesNames("lower, upper");
    Overlay;
  >]
  let dchan period (h:float[]) l =
    let lookbackTotal = dchanLookback period
    let startIdx = lookbackTotal
    let endIdx = h.Length - 1
    Study.lazyCompute2 startIdx endIdx (fun outLen ->
      (Stat.min period l, Stat.max period h)
    )

  //keltner bands
  let kbandsLookback (ma:MA) period =
    max (ma.lookback period + Price.typLookback) (Volatility.atrLookback period)

  [<TradingStudy;
    Group("Bands");
    Title("Keltner Bands");
    Description("Returns the Keltner bands");
    OutputSeriesNames("lower, middle, upper");
    Overlay;
  >]
  let kbands (ma:MA) period devUp devDown (h:float[]) l c =
    Study.checkPositiveIntMin1 period
    Study.checkPositiveReal devUp
    Study.checkPositiveReal devDown
    Study.checkHighLowClose h l c
    let lookbackTotal = kbandsLookback ma period
    let startIdx = lookbackTotal
    let endIdx = h.Length - 1
    Study.lazyCompute3 startIdx endIdx (fun outLen ->
      let typ = Price.typ h l c
      let mid = ma.create period typ
      let atr = Volatility.atr period h l c
      let offset = atr.Length - mid.Length
      if offset > 0 then
        let h = Array.init mid.Length (fun i -> mid.[i] + atr.[i+offset] * devUp)
        let l = Array.init mid.Length (fun i -> mid.[i] - atr.[i+offset] * devDown)
        (l, mid, h)
      else
        //offset is negative, so we substract it
        let h = Array.init atr.Length (fun i -> mid.[i-offset] + atr.[i] * devUp)
        let l = Array.init atr.Length (fun i -> mid.[i-offset] - atr.[i] * devDown)
        (h, mid, l)
    )
