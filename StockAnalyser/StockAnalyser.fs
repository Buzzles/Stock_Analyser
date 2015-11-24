#light
module StockAnaylser
open System.Net
open System.IO
open FSharp.Charting

    ////
    ////let grid (prices : seq<System.DateTime * float>) =
    ////    let form = new Form(Visible = true, TopMost = true)
    ////    let grid = new DataGridVeiw(Dock = DockStyle.Fill, Visible = True)
    ////    form.Controls.Add(grid)
    ////    grid.DataSource <- prices |> Seq.toArray

let internal loadPrices ticker =
    // LLOY.L
    //let ticker = "msft" 
    let url = "http://real-chart.finance.yahoo.com/table.csv?s=" + ticker + "&d=10&e=9&f=2015&g=d&a=11&b=28&c=1995&ignore=.csv"

    // Note, like C#
    let request = WebRequest.Create(url)
    let response = request.GetResponse()
    let stream = response.GetResponseStream()
    let reader = new StreamReader(stream) // System.IO.StreamReader is C#!
    let csv = reader.ReadToEnd() // big string

    let prices = 
        csv.Split([|'\n'|])
        |> Seq.skip 1 // header
        |> Seq.map (fun line -> line.Split([|','|]))
        |> Seq.filter (fun values -> values |> Seq.length = 7)
        |> Seq.map (fun values ->
                    System.DateTime.Parse(values.[0]),
                    float values.[6])
    prices

type StockAnalyser (lprices, days) =
    let prices =
        lprices
        |> Seq.map snd
        |> Seq.take days
    static member GetAnalysers (tickers, days) =
        tickers
        |> Seq.map loadPrices
        |> Seq.map (fun prices ->  new StockAnalyser(prices, days))
    member s.Return = //Daily return 
        let lastPrice = prices |> Seq.nth 0
        let startPrice = prices |> Seq.nth (days - 1)
        lastPrice / startPrice - 1.
    member s.StdDev = // Standard Deviation (log of return)
        let logRets =
            prices
            |> Seq.pairwise
            |> Seq.map (fun (x, y) -> log (x / y))
        let mean = logRets |> Seq.average
        let sqr x = x * x
        let variance = logRets |> Seq.averageBy (fun r -> sqr(r - mean))
        sqrt variance
////        FSharp.Charting.Chart.Line(var)