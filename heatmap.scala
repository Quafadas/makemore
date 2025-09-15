
import viz.LowPriorityPlotTarget

def heatmap(data: collection.Seq[(String, Int)])(using LowPriorityPlotTarget) =
  import viz.Plottable.*
  import viz.vegaFlavour
  val spec =  os.resource / "HeatmapBigram.vg.json"
  spec.plot(
    Seq(
      (spec: ujson.Value) =>
        spec("data")("values") = data.map((a, b) =>
          ujson.Obj(
            "first" -> a.charAt(0).toString(),
            "second" -> a.charAt(1).toString(),
            "value" -> b
          )
        ),
      viz.Utils.fillDiv
    )
  )
