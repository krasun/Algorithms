// https://en.wikipedia.org/wiki/Longest_common_subsequence_problem

def computeLCSTable(left: String, right: String): Array[Array[Int]] = {
  val lcsTable = new Array[Array[Int]](left.length + 1)
  for (leftIndex <- lcsTable.indices) {
    lcsTable(leftIndex) = new Array[Int](right.length + 1)
  }

  for (leftIndex0 <- left.indices; leftIndex1 = leftIndex0 + 1) {
    for (rightIndex0 <- right.indices; rightIndex1 = rightIndex0 + 1) {
      lcsTable(leftIndex1)(rightIndex1) =
        if (left(leftIndex0) == right(rightIndex0)) {
          lcsTable(leftIndex1 - 1)(rightIndex1 - 1) + 1
        } else {
          math.max(lcsTable(leftIndex1)(rightIndex1 - 1), lcsTable(leftIndex1 - 1)(rightIndex1))
        }
    }
  }

  lcsTable
}

def assembleLCS(left: String, right: String, lcsTable: Array[Array[Int]], leftIndex0: Int, rightIndex0: Int): String = {
  val (leftIndex1, rightIndex1) = (leftIndex0 + 1, rightIndex0 + 1)

  if (0 == lcsTable(leftIndex1)(rightIndex1)) {
    ""
  } else if (left(leftIndex0) == right(rightIndex0)) {
    assembleLCS(left, right, lcsTable, leftIndex0 - 1, rightIndex0 - 1) + left(leftIndex0)
  } else if (lcsTable(leftIndex1)(rightIndex1 - 1) > lcsTable(leftIndex1 - 1)(rightIndex1)) {
    assembleLCS(left, right, lcsTable, leftIndex0, rightIndex0 - 1)
  } else {
    assembleLCS(left, right, lcsTable, leftIndex0 - 1, rightIndex0)
  }
}

def computeLCS(left: String, right: String): String = assembleLCS(left, right, computeLCSTable(left, right), left.length - 1, right.length - 1)

println(computeLCS("CATCGA", "GTACCGTCA") == "CTCA")