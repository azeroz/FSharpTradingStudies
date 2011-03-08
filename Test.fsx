#r @"..\bin\Debug\TradingStudies.dll"
open System
open Trading.Data.DataLoader

printfn "Test %A" (Yahoo.create  ) 


// Main - start the application  
//[<STAThread>]
//do for KeyValue(name, info) in Trading.Chart.Reflection.getStudiesInfo()  do
//    printfn "%s : %A" name info
