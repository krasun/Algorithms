// https://en.wikipedia.org/wiki/Quicksort

def quickSort(array: Array[Int]): Unit = {

  def swap(array: Array[Int], replaceWithIndex: Int, targetIndex: Int): Unit = {
    val temporary = array(targetIndex)
    array(targetIndex) = array(replaceWithIndex)
    array(replaceWithIndex) = temporary
  }

  def partition(array: Array[Int], startIndex: Int, endIndex: Int): Int = {
    var leftEndIndex = startIndex
    var pivotIndex = endIndex

    for (unknownStartIndex <- startIndex to (endIndex - 1)) {
      val pivot = array(pivotIndex)

      if (array(unknownStartIndex) <= pivot) {
        swap(array, leftEndIndex, unknownStartIndex)

        leftEndIndex = leftEndIndex + 1
      }
    }

    // pivot now is at the left index
    swap(array, leftEndIndex, pivotIndex)

    return leftEndIndex
  }

  def doQuickSort(array: Array[Int], startIndex: Int, endIndex: Int): Unit = {
    if (startIndex >= endIndex) {
      return
    }

    val pivotIndex = partition(array, startIndex, endIndex)

    doQuickSort(array, startIndex, pivotIndex - 1)
    doQuickSort(array, pivotIndex + 1, endIndex)
  }

  doQuickSort(array, 0, array.length - 1)
}


val array = Array(7, 6, 5, 4, 3, 2, 1)
quickSort(array)

println(
  array.sameElements(Array(1, 2, 3, 4, 5, 6, 7))
)