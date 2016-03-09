// https://en.wikipedia.org/wiki/Topological_sorting
// (u, v) is u -> v

import scala.collection.mutable

def topologicalSort(g: Array[Array[Int]]): Seq[Int] = {
  var sorted = new mutable.MutableList[Int]

  // compute number of incoming edges for every vertex
  val inDegree = new Array[Int](g.length)
  for (u <- g.indices; v <- g(u).indices; if 1 == g(u)(v)) {
    inDegree(v) += 1
  }

  // define vertices for processing - without incoming edges
  val next = mutable.Stack[Int]((for (u <- inDegree.indices; if 0 == inDegree(u)) yield u): _*)

  while (next.nonEmpty) {
    val u = next.pop()

    // append vertex without incoming edges to sorted list
    sorted += u

    // decrease number of incoming edges for every edge (u -> v)
    for (v <- g(u).indices if 1 == g(u)(v)) {
      inDegree(v) -= 1
      // add vertex to processing if there are no incoming edges
      if (0 == inDegree(v)) {
        next.push(v)
      }
    }
  }

  sorted
}

val graph = Array(
  Array(0, 0, 1, 0, 0, 0),
  Array(0, 0, 0, 1, 1, 0),
  Array(0, 0, 0, 0, 0, 1),
  Array(0, 0, 0, 0, 0, 1),
  Array(0, 0, 0, 0, 0, 1),
  Array(0, 0, 0, 0, 0, 0)
)

val sequence = topologicalSort(graph)

println(sequence.mkString(" "))
println(sequence.equals(Array(0, 2, 1, 4, 3, 5).toSeq))