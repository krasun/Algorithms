// https://en.wikipedia.org/wiki/Topological_sorting

import scala.collection.mutable

def topologicalSort(g: Array[Array[Int]]): Seq[Int] = {
  var sorted = new mutable.MutableList[Int]

  val inDegree = new Array[Int](g.length)
  // (u, v) is u -> v
  for (u <- g.indices; v <- g(u).indices; if 1 == g(u)(v)) {
    inDegree(v) += 1
  }

  val next = mutable.Stack[Int]((for (u <- inDegree.indices; if 0 == inDegree(u)) yield u): _*)

  while (next.nonEmpty) {
    val u = next.pop()

    sorted += u

    for (v <- g(u).indices if 1 == g(u)(v)) {
      inDegree(v) -= 1
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