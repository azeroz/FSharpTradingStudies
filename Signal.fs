namespace Trading.Studies

module Signal =

  let fisherTAux x =
    0.5 * log ((1.0 + x) / (1.0 - x))

  let fisherTLookback = 0

  let fisherT (data:float[]) =
    let lookbackTotal = fisherTLookback
    let startIdx = lookbackTotal
    let endIdx = data.Length - 1
    Study.lazyCompute startIdx endIdx (fun outLen ->
      data
        |> Study.toRange (-0.99999) 0.99999
        |> Array.map fisherTAux
    )

  let fisherInv x =
    let x = exp (2.0 * x)
    (x - 1.0) / (x + 1.0)

  let fisherInvTLookback = 0

  let fisherInvT (data:float[]) =
    let lookbackTotal = fisherInvTLookback
    let startIdx = lookbackTotal
    let endIdx = data.Length - 1
    Study.lazyCompute startIdx endIdx (fun outLen ->
      data
        |> Study.toRange (-0.99999) 0.99999
        |> Array.map fisherInv
    )
