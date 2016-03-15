// https://en.wikipedia.org/wiki/Bellman%E2%80%93Ford_algorithm

def bellmanFord(graph: Array[Array[Int]], startVertex: Int): (Array[Double], Array[Option[Int]]) = {

  def initializePaths(vertices: Range, startVertex: Int): (Array[Double], Array[Option[Int]]) = {
    val shortest = new Array[Double](vertices.length)
    for (vertex <- shortest.indices) {
      // it's not correct, but Int.MaxValue is used as infinity
      shortest(vertex) = if (vertex == startVertex) 0 else Double.PositiveInfinity
    }

    val prev = new Array[Option[Int]](vertices.length)
    for (vertex <- prev.indices) {
      prev(vertex) = None
    }

    (shortest, prev)
  }

  def edges(graph: Array[Array[Int]]): Seq[(Int, Int)] =
    for (u <- graph.indices; v <- graph(u).indices; if 0 != graph(u)(v))
      yield (u, v)

  def relax(graph: Array[Array[Int]], u: Int, v: Int, shortest: Array[Double], prev: Array[Option[Int]]) {
    val weight = graph(u)(v)
    if ((shortest(u) + weight) < shortest(v)) {
      shortest(v) = shortest(u) + weight
      prev(v) = Some(u)
    }
  }

  val (shortest, prev) = initializePaths(graph.indices, startVertex)

  for (i <- 0 until (graph.length - 1)) {
    for ((u, v) <- edges(graph)) {
      relax(graph, u, v, shortest, prev)
    }
  }

  (shortest, prev)
}

val graph = Array(
  Array(0, 6, 7, 0, 0),
  Array(0, 0, 8, 5, -4),
  Array(0, 0, 0, -3, 9),
  Array(0, -2, 0, 0, 0),
  Array(2, 0, 0, 7, 0)
)

val (shortest, prev) = bellmanFord(graph, 0)

println(shortest.mkString(" "))
println(prev.mkString(" "))

println(shortest.sameElements(Array(0.0, 2.0, 7.0, 4.0, -2.0)))
println(prev.sameElements(Array(None, Some(3), Some(0), Some(2), Some(1))))
