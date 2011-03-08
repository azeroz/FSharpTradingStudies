namespace Trading.Studies

module Price =

  let medLookback = 0

  [<TradingStudy;
    Group("Price");
    Title("Median Price");
    Description("Returns (High + Low) / 2");
    Overlay;
  >]
  let med high low =
    Study.checkHighLow high low
    let lookbackTotal = medLookback
    let startIdx = lookbackTotal
    let endIdx = high.Length - 1
    Study.lazyCompute startIdx endIdx (fun outLen ->
      Array.init outLen (fun i ->
        let today = startIdx+i
        (high.[today] + low.[today]) / 2.0
      )
    )

  //=====================================================

  let typLookback = 0

  [<TradingStudy;
    Group("Price");
    Title("Typical Price");
    Description("Returns (High + Low + Close) / 3");
    Overlay;
  >]
  let typ high low close =
    Study.checkHighLowClose high low close
    let lookbackTotal = typLookback
    let startIdx = lookbackTotal
    let endIdx = high.Length - 1
    Study.lazyCompute startIdx endIdx (fun outLen ->
      Array.init outLen (fun i ->
        let today = startIdx+i
        (high.[today] + low.[today] + close.[today]) / 3.0
      )
    )

  //=====================================================       

  let avgLookback = 0

  [<TradingStudy;
    Group("Price");
    Title("Average Price");
    Description("Returns (Open + High + Low + Close) / 4");
    Overlay;
  >]
  let avg o high low close =
    Study.checkOpenHighLowClose o high low close
    let lookbackTotal = avgLookback
    let startIdx = lookbackTotal
    let endIdx = high.Length - 1
    Study.lazyCompute startIdx endIdx (fun outLen ->
      Array.init outLen (fun i ->
        let today = startIdx+i
        (o.[today] + high.[today] + low.[today] + close.[today]) / 4.0
      )
    )

  //=====================================================       

  let wclLookback = 0

  [<TradingStudy;
    Group("Price");
    Title("Weighted Close Price");
    Description("Returns (High + Low + 2xClose) / 4");
    Overlay;
  >]
  let wcl high low close =
    Study.checkHighLowClose high low close
    let lookbackTotal = wclLookback
    let startIdx = lookbackTotal
    let endIdx = high.Length - 1
    Study.lazyCompute startIdx endIdx (fun outLen ->
      Array.init outLen (fun i ->
        let today = startIdx+i
        (high.[today] + low.[today] + 2.0 * close.[today]) / 4.0
      )
    )

  //=====================================================

  let midLookback period = Stat.maxLookback period

  [<TradingStudy;
    Group("Price");
    Title("Mid Price");
    Description("Returns (Highest high + Lowest low) / 2");
    Overlay;
  >]
  let mid period high low =
    Study.checkPositiveIntMin1 period
    Study.checkHighLow high low
    let lookbackTotal = midLookback period
    let startIdx = lookbackTotal
    let endIdx = high.Length - 1
    Study.lazyCompute startIdx endIdx (fun outLen ->
      let hh = Stat.max period high
      let ll = Stat.min period low
      Array.map2 (fun x y -> (x+y) / 2.0) hh ll
    )

  //=====================================================

  let midPointLookback period = midLookback period

  [<TradingStudy;
    Group("Price");
    Title("Midpoint Price");
    Description("Same as mid price : returns (Highest high + Lowest low) / 2");
    Overlay;
  >]
  let midPoint period data =
    mid period data data
