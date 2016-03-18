// https://en.wikipedia.org/wiki/Floyd–Warshall_algorithm

def floydWarshall(graph: Array[Array[Double]]): (Array[Array[Array[Double]]], Array[Array[Array[Option[Double]]]]) = {

  def initializePaths(graph: Array[Array[Double]]): (Array[Array[Array[Double]]], Array[Array[Array[Option[Double]]]]) = {
    val vertexVolume = graph.length

    val shortest = new Array[Array[Array[Double]]](vertexVolume)
    val prev = new Array[Array[Array[Option[Double]]]](vertexVolume)
    for (u <- graph.indices) {
      shortest(u) = new Array[Array[Double]](vertexVolume)
      prev(u) = new Array[Array[Option[Double]]](vertexVolume)

      for (v <- graph.indices) {
        shortest(u)(v) = new Array[Double](vertexVolume + 1)
        prev(u)(v) = new Array[Option[Double]](vertexVolume + 1)

        shortest(u)(v)(0) = graph(u)(v)
        prev(u)(v)(0) = if (Double.PositiveInfinity != graph(u)(v)) Option(u) else None
      }
    }

    (shortest, prev)
  }

  val (shortest, prev) = initializePaths(graph)
  for (x0 <- graph.indices; x = x0 + 1) {
    for (u <- graph.indices) {
      for (v <- graph.indices) {
        if (shortest(u)(v)(x) < (shortest(u)(x)(x - 1) + shortest(x)(v)(x - 1))) {
          shortest(u)(v)(x) = shortest(u)(x)(x - 1) + shortest(x)(v)(x - 1)
          prev(u)(v)(x) = prev(x)(v)(x - 1)
        } else {
          shortest(u)(v)(x) = shortest(u)(v)(x - 1)
          prev(u)(v)(x) = prev(u)(v)(x - 1)
        }
      }
    }
  }

  (shortest, prev)
}

// adjacency matrix
val graph = Array(
  Array(0, 3, 8, Double.PositiveInfinity),
  Array(Double.PositiveInfinity, 0, Double.PositiveInfinity, 1),
  Array(Double.PositiveInfinity, 4, 0, Double.PositiveInfinity),
  Array(2, Double.PositiveInfinity, -5, 0)
)

val (shortest, prev) = floydWarshall(graph)

for (u <- shortest.indices) {
  for (v <- shortest(u).indices) {
    print("\t ", shortest(u)(v)(shortest.length))
  }
  println()
}

for (u <- prev.indices) {
  for (v <- prev(u).indices) {
    print("\t ", prev(u)(v)(prev.length))
  }
  println()
}

//println(shortest.sameElements(
//
//))
//
//println(prev.sameElements(
//
//))
