// https://en.wikipedia.org/wiki/Bellman-Ford_algorithm

import scala.collection.mutable

def bellmanFord(graph: Array[Array[Int]], startVertex: Int): (Array[Double], Array[Option[Int]], Seq[Int]) = {

  def initializePaths(vertices: Range, startVertex: Int): (Array[Double], Array[Option[Int]]) = {
    val shortest = new Array[Double](vertices.length)
    for (vertex <- shortest.indices) {
      // it's not correct, but Int.MaxValue is used as infinity
      shortest(vertex) = if (vertex == startVertex) 0 else Double.PositiveInfinity
    }

    val pred = new Array[Option[Int]](vertices.length)
    for (vertex <- pred.indices) {
      pred(vertex) = None
    }

    (shortest, pred)
  }

  def relax(graph: Array[Array[Int]], u: Int, v: Int, shortest: Array[Double], pred: Array[Option[Int]]) {
    val weight = graph(u)(v)
    if ((shortest(u) + weight) < shortest(v)) {
      shortest(v) = shortest(u) + weight
      pred(v) = Some(u)
    }
  }

  def edges(graph: Array[Array[Int]]): Seq[(Int, Int)] =
    for (u <- graph.indices; v <- graph(u).indices; if 0 != graph(u)(v))
      yield (u, v)


  def findNegativeWeightCycle(graph: Array[Array[Int]], shortest: Array[Double], pred: Array[Option[Int]]): Seq[Int] = {
    var cycle = mutable.MutableList[Int]()

    for ((u, v) <- edges(graph); if (shortest(u) + graph(u)(v)) < shortest(v)) {
      val visited = new Array[Boolean](graph.length)

      var x = v
      while (! visited(x)) {
        visited(x) = true
        x = pred(x).get
      }

      var vertexInCycle = pred(x).get
      cycle += x
      while (vertexInCycle != x) {
        cycle = mutable.MutableList[Int](vertexInCycle) ++ cycle
        vertexInCycle = pred(vertexInCycle).get
      }

      return cycle
    }

    return cycle
  }

  val (shortest, pred) = initializePaths(graph.indices, startVertex)

  for (i <- 0 until (graph.length - 1)) {
    for ((u, v) <- edges(graph)) {
      relax(graph, u, v, shortest, pred)
    }
  }

  (shortest, pred, findNegativeWeightCycle(graph, shortest, pred))
}


// adjacency matrix, zero is used instead of infinity for readability
val graph = Array(
  Array(0, 6, 7, 0, 0),
  Array(0, 0, 8, 5, -4),
  Array(0, 0, 0, -3, 9),
  Array(0, -2, 0, 0, 0),
  Array(2, 0, 0, 7, 0)
)

val (shortest, pred, emptyNegativeWeightCycle) = bellmanFord(graph, 0)

println("Results without negative cycle:")

println(shortest.mkString(" "))
println(pred.mkString(" "))
println(emptyNegativeWeightCycle.mkString(" "))

println(emptyNegativeWeightCycle.isEmpty)
println(shortest.sameElements(Array(0.0, 2.0, 7.0, 4.0, -2.0)))
println(pred.sameElements(Array(None, Some(3), Some(0), Some(2), Some(1))))


val graphWithNegativeCycle = Array(
  Array(0, 1, 0),
  Array(0, 0, -1),
  Array(1, -1, 0)
)

val (shortestNegativeCycle, predNegativeCycle, negativeWeightCycle) = bellmanFord(graphWithNegativeCycle, 0)

println("Results with negative cycle:")

println(shortestNegativeCycle.mkString(" "))
println(predNegativeCycle.mkString(" "))
println(negativeWeightCycle.mkString(" "))

println(negativeWeightCycle.isEmpty)
println(shortestNegativeCycle.sameElements(Array(0.0, 2.0, 7.0, 4.0, -2.0)))
println(predNegativeCycle.sameElements(Array(None, Some(3), Some(0), Some(2), Some(1))))
