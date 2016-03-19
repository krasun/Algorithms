// https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm

import scala.collection.mutable

def dijkstraAlgorithm(graph: Array[Array[Int]], startVertex: Int): (Array[Double], Array[Option[Int]]) = {

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

  val (shortest, pred) = initializePaths(graph.indices, startVertex)

  var q = mutable.MutableList(graph.indices: _*)
  while (q.nonEmpty) {
    var vertexWithMinWeight = q.head
    for (vertex <- q) {
      if (shortest(vertex) < shortest(vertexWithMinWeight)) {
        vertexWithMinWeight = vertex
      }
    }

    q = q.filterNot(_ == vertexWithMinWeight)

    for (adjacentVertex <- graph(vertexWithMinWeight).indices if 0 != graph(vertexWithMinWeight)(adjacentVertex)) {
      relax(graph, vertexWithMinWeight, adjacentVertex, shortest, pred)
    }
  }

  (shortest, pred)
}

// adjacency matrix, zero is used instead of infinity for readability
val graph = Array(
  Array(0, 6, 4, 0, 0),
  Array(0, 0, 2, 3, 0),
  Array(0, 1, 0, 9, 3),
  Array(0, 0, 0, 0, 4),
  Array(7, 0, 0, 5, 0)
)

val (shortest, pred) = dijkstraAlgorithm(graph, 0)

println(shortest.mkString(" "))
println(pred.mkString(" "))

println(shortest.sameElements(Array(0.0, 5.0, 4.0, 8.0, 7.0)))
println(pred.sameElements(Array(None, Some(2), Some(0), Some(1), Some(2))))
