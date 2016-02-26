// https://en.wikipedia.org/wiki/Quicksort

def quickSort(array: Array[Int]): Unit = {

  def exchange(array: Array[Int], replaceWithIndex: Int, targetIndex: Int): Unit = {
    val temporary = array(targetIndex)
    array(targetIndex) = array(replaceWithIndex)
    array(replaceWithIndex) = temporary
  }

  def partition(array: Array[Int], startIndex: Int, endIndex: Int): Int = {
    var pivotIndex = startIndex

    for (index <- startIndex to (endIndex - 1)) {
      if (array(index) <= array(endIndex)) {
        exchange(array, pivotIndex, index)

        pivotIndex = pivotIndex + 1
      }
    }

    exchange(array, pivotIndex, endIndex)

    return pivotIndex
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