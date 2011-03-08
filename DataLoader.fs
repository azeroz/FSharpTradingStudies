namespace Trading.Data

open System
open System.Net
open Microsoft.FSharp.Control.WebExtensions

open FileHelpers
open FileHelpers.DataLink

open Trading.Data

type DataLoader =
  interface
    abstract AsyncDownload : string -> DateTime -> DateTime -> Async<PriceSeries>
  end

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module DataLoader =

  module Excel =

    [<DelimitedRecord("|")>]
    type PriceSample () =
      [<DefaultValue>]
      val mutable Date : DateTime
      [<DefaultValue>]
      val mutable Open : float
      [<DefaultValue>]
      val mutable High : float
      [<DefaultValue>]
      val mutable Low : float
      [<DefaultValue>]
      val mutable Close : float
      [<DefaultValue>]
      val mutable Volume : float

    let create tickerDir ext startRow startColumn =
      {new DataLoader with
        override x.AsyncDownload ticker first last =
          async {
            let fullPath = System.IO.Path.Combine(tickerDir, ticker + ext)
            let provider =
              new ExcelStorage(
                typeof<PriceSample>,
                StartRow=startRow,
                StartColumn=startColumn,
                FileName=fullPath
              )
            let res =
              provider.ExtractRecords()
                |> unbox<PriceSample[]>
                |> Array.filter (fun sample -> sample.Date >= first && sample.Date <= last)
                |> Array.sortBy (fun sample -> sample.Date)

            let series = PriceSeries.create res.Length

            res |> Array.iteri (fun i sample ->
              series.Date.[i] <- sample.Date
              series.Open.[i] <- sample.Open
              series.High.[i] <- sample.High
              series.Low.[i] <- sample.Low
              series.Close.[i] <- sample.Close
              series.Volume.[i] <- sample.Volume
            )

            PriceSeries.checkDates series.Date
            PriceSeries.checkOpenHighLowClose series.Open series.High series.Low series.Close

            return series
          }
      }

  module Yahoo =

    let createUrl ticker (first:DateTime) (last:DateTime) =
        sprintf
          @"http://ichart.finance.yahoo.com/table.csv?s=%s&a=%d&b=%d&c=%d&d=%d&e=%d&f=%d&g=d&ignore=.csv"
            ticker
            (first.Month - 1) first.Day first.Year
            (last.Month - 1) last.Day last.Year 

    let asyncDownload url =
      async {
        let client = new WebClient()
        return! client.AsyncDownloadString(url)
      }

    let create () =
      {new DataLoader with
        override x.AsyncDownload ticker first last =
          async {
            let url =  Uri(createUrl ticker first last)
            let! data = asyncDownload url
            let lines = data.Split( [|'\n'|]).[1..]
            let series =  PriceSeries.create lines.Length
            let firstOk = ref -1
            let counter = ref 0
            for line in 0 .. lines.Length - 1 do
              let columns = lines.[line].Split([|','|])
              if Array.length columns = 7 then
                let date = DateTime.Parse columns.[0]
                if date >= first && date <= last then
                  if !firstOk < 0 then firstOk := line
                  incr counter
                  series.Date.[line] <- date
                  series.Open.[line] <- float columns.[1]
                  series.High.[line] <- float columns.[2]
                  series.Low.[line] <- float columns.[3]
                  //adjusted close
                  //normal close would be the 4th column
                  series.Close.[line] <- float columns.[6]
                  series.Volume.[line] <- float columns.[5]
            //Delete invalid dates
            if !counter <> lines.Length then
              let newSeries =  PriceSeries.create !counter
              Array.blit series.Date !firstOk newSeries.Date 0 !counter
              Array.blit series.Open !firstOk newSeries.Open 0 !counter
              Array.blit series.High !firstOk newSeries.High 0 !counter
              Array.blit series.Low !firstOk newSeries.Low 0 !counter
              Array.blit series.Close !firstOk newSeries.Close 0 !counter
              Array.blit series.Volume !firstOk newSeries.Volume 0 !counter
              return newSeries
            else
              return series
          }
      }

