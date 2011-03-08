namespace Trading.Studies

module internal BaseStudies =

  let wmomLookback (period:int) = period

  let wmom coeffToday coeffLag period (data:float[]) =
    Study.checkPositiveIntMin1 period
    let lookbackTotal = wmomLookback period
    let startIdx = lookbackTotal
    let endIdx = data.Length - 1
    Study.lazyCompute startIdx endIdx (fun outLen ->
      let f =
        match coeffToday, coeffLag with
        | 0.0, 0.0 -> fun today -> 0.0
        | 0.0, 1.0 -> fun today -> -data.[today - lookbackTotal]
        | 0.0, beta -> fun today -> -beta * data.[today - lookbackTotal]
        | 1.0, 0.0 -> fun today -> data.[today]
        | 1.0, 1.0 -> fun today -> data.[today] - data.[today - lookbackTotal]
        | alpha, 0.0 -> fun today -> alpha * data.[today]
        | alpha, beta -> fun today -> alpha * data.[today] - beta * data.[today - lookbackTotal]

      Array.init outLen (fun i -> f (startIdx + i))
    )

  let momLookback period = wmomLookback period

  let mom period data = wmom  1.0 1.0 period data

  let rocLookback (period:int) = period

  let roc isBase100 divByZeroDefaultValue period (data:float[]) =
    Study.checkPositiveIntMin1 period
    let lookbackTotal = wmomLookback period
    let startIdx = lookbackTotal
    let endIdx = data.Length - 1
    Study.lazyCompute startIdx endIdx (fun outLen ->
      if isBase100 then
        Array.init outLen (fun i ->
          let today = startIdx + i
          let lagValue = today - period
          if data.[lagValue] <> 0.0 then
            100.0 * (data.[today] / data.[lagValue] - 1.0)
          else
            divByZeroDefaultValue
        )
      else
        Array.init outLen (fun i ->
          let today = startIdx + i
          let lagValue = today - period
          if data.[lagValue] <> 0.0 then
            data.[today] / data.[lagValue] - 1.0
          else
            divByZeroDefaultValue
        )
    )