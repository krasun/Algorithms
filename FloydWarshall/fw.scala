// https://en.wikipedia.org/wiki/Floyd–Warshall_algorithm

import scala.reflect.ClassTag

def floydWarshall(graph: Array[Array[Double]]): (Array[Array[Array[Double]]], Array[Array[Array[Option[Int]]]]) = {

  def initializePaths(graph: Array[Array[Double]]): (Array[Array[Array[Double]]], Array[Array[Array[Option[Int]]]]) = {
    val vertexVolume = graph.length

    val shortest = new Array[Array[Array[Double]]](vertexVolume)
    val pred = new Array[Array[Array[Option[Int]]]](vertexVolume)
    for (u <- graph.indices) {
      shortest(u) = new Array[Array[Double]](vertexVolume)
      pred(u) = new Array[Array[Option[Int]]](vertexVolume)

      for (v <- graph.indices) {
        shortest(u)(v) = new Array[Double](vertexVolume + 1)
        pred(u)(v) = new Array[Option[Int]](vertexVolume + 1)

        shortest(u)(v)(0) = graph(u)(v)
        pred(u)(v)(0) = if (Double.PositiveInfinity != graph(u)(v) && 0 != graph(u)(v)) Option(u) else None

        for (x <- 1 to vertexVolume) {
          shortest(u)(v)(x) = Double.PositiveInfinity
          pred(u)(v)(x) = None
        }
      }
    }

    (shortest, pred)
  }

  val (shortest, pred) = initializePaths(graph)

  for (x0 <- graph.indices; x1 = x0 + 1) {
    for (u <- graph.indices) {
      for (v <- graph.indices) {
        if (shortest(u)(v)(x1 - 1) <= (shortest(u)(x0)(x1 - 1) + shortest(x0)(v)(x1 - 1))) {
          shortest(u)(v)(x1) = shortest(u)(v)(x1 - 1)
          pred(u)(v)(x1) = pred(u)(v)(x1 - 1)
        } else {
          shortest(u)(v)(x1) = shortest(u)(x0)(x1 - 1) + shortest(x0)(v)(x1 - 1)
          pred(u)(v)(x1) = pred(x0)(v)(x1 - 1)
        }
      }
    }
  }

  (shortest, pred)
}

def transformTo2d[A](array3d: Array[Array[Array[A]]], index: Int)(implicit classTag: ClassTag[A]): Array[Array[A]] = {
  val array2d = new Array[Array[A]](array3d.length)
  for (u <- array3d.indices) {
    array2d(u) = new Array(array3d(u).length)
    for (v <- array3d(u).indices) {
      array2d(u)(v) = array3d(u)(v)(index)
    }
  }

  array2d
}

def printPaths[A](paths: Array[Array[Array[A]]], index: Int): Unit = {
  for (u <- paths.indices) {
    for (v <- paths(u).indices) {
      print("\t" + (if (paths(u)(v)(index) == Double.PositiveInfinity) "\u221E" else paths(u)(v)(index)))
    }
    println()
  }
}

def equals[A](left: Array[Array[A]], right: Array[Array[A]]): Boolean = {
  val these = left.iterator
  val those = right.iterator
  while (these.hasNext && those.hasNext)
    if (! these.next.sameElements(those.next))
      return false

  !these.hasNext && !those.hasNext
}

// adjacency matrix
val graph = Array(
  Array(0, 3, 8, Double.PositiveInfinity),
  Array(Double.PositiveInfinity, 0, Double.PositiveInfinity, 1),
  Array(Double.PositiveInfinity, 4, 0, Double.PositiveInfinity),
  Array(2, Double.PositiveInfinity, -5, 0)
)

val (shortest, pred) = floydWarshall(graph)

println("Weights: ")
printPaths(shortest, graph.length)
println("Predecessors: ")
printPaths(pred, graph.length)


println(equals[Double](transformTo2d(shortest, graph.length),
  Array(
    Array(0.0, 3.0, -1.0, 4.0),
    Array(3.0, 0.0, -4.0, 1.0),
    Array(7.0, 4.0, 0.0, 5.0),
    Array(2.0, -1.0, -5.0, 0.0)
  )
))

println(equals[Option[Int]](transformTo2d(pred, graph.length),
  Array(
    Array(None, Option(0), Option(3), Option(1)),
    Array(Option(3), None, Option(3), Option(1)),
    Array(Option(3), Option(2), None, Option(1)),
    Array(Option(3), Option(2), Option(3), None)
  )
))