// https://xlinux.nist.gov/dads//HTML/dagShortPath.html

import scala.collection.mutable

def dagShortestPaths(graph: Array[Array[Int]], startVertex: Int): (Array[Double], Array[Option[Int]]) = {

  def topologicalSort(g: Array[Array[Int]]): Seq[Int] = {
    var sorted = new mutable.MutableList[Int]

    // compute number of incoming edges for every vertex
    val inDegree = new Array[Int](g.length)
    for (u <- g.indices; v <- g(u).indices; if 0 != g(u)(v)) {
      inDegree(v) += 1
    }

    // define vertices for processing - without incoming edges
    val next = mutable.Stack[Int]((for (u <- inDegree.indices; if 0 == inDegree(u)) yield u): _*)

    while (next.nonEmpty) {
      val u = next.pop()

      // append vertex without incoming edges to sorted list
      sorted += u

      // decrease number of incoming edges for every edge (u -> v)
      for (v <- g(u).indices if 0 != g(u)(v)) {
        inDegree(v) -= 1
        // add vertex to processing if there are no incoming edges
        if (0 == inDegree(v)) {
          next.push(v)
        }
      }
    }

    sorted
  }

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

  def relax(graph: Array[Array[Int]], u: Int, v: Int, shortest: Array[Double], prev: Array[Option[Int]]) {
    val weight = graph(u)(v)
    if ((shortest(u) + weight) < shortest(v)) {
      shortest(v) = shortest(u) + weight
      prev(v) = Some(u)
    }
  }

  val sorted = topologicalSort(graph)
  val (shortest, prev) = initializePaths(graph.indices, startVertex)


  for (u <- sorted) {
    for (v <- graph(u).indices if 0 != graph(u)(v)) {
      relax(graph, u, v, shortest, prev)
    }
  }

  (shortest, prev)
}

val graph = Array(
  Array(0, 5, 3, 0, 0, 0),
  Array(0, 0, 2, 6, 0, 0),
  Array(0, 0, 0, 7, 4, 2),
  Array(0, 0, 0, 0, -1, 1),
  Array(0, 0, 0, 0, 0, -2),
  Array(0, 0, 0, 0, 0, 0)
)

val (shortest, prev) = dagShortestPaths(graph, 1)

println(shortest.mkString(" "))
println(prev.mkString(" "))

println(shortest.sameElements(Array(Double.PositiveInfinity, 0.0, 2.0, 6.0, 5.0, 3.0)))
println(prev.sameElements(Array(None, None, Some(1), Some(1), Some(3), Some(4))))
