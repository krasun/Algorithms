// https://en.wikipedia.org/wiki/Quicksort

def quickSort(array: Array[Int]): Unit = {

  def swap(array: Array[Int], replaceWithIndex: Int, targetIndex: Int): Unit = {
    val temporary = array(targetIndex)
    array(targetIndex) = array(replaceWithIndex)
    array(replaceWithIndex) = temporary
  }

  def partition(array: Array[Int], startIndex: Int, endIndex: Int): Int = {
    // index of end of elements of left group (where all elements are less than pivot)
    var rightStartIndex = startIndex
    var pivotIndex = endIndex

    // unknown start index is start index of unknown group
    for (unknownStartIndex <- startIndex to (endIndex - 1)) {
      val pivot = array(pivotIndex)

      if (array(unknownStartIndex) <= pivot) {
        swap(array, rightStartIndex, unknownStartIndex)

        rightStartIndex = rightStartIndex + 1
      }
    }

    // pivot now is at the left index
    swap(array, rightStartIndex, pivotIndex)

    return rightStartIndex
  }

  def doQuickSort(array: Array[Int], startIndex: Int, endIndex: Int): Unit = {
    if (startIndex >= endIndex) {
      return
    }

    // rearrange elements around pivot and return pivot index
    val pivotIndex = partition(array, startIndex, endIndex)

    // sort elements at the left hand of pivot
    doQuickSort(array, startIndex, pivotIndex - 1)
    // sort elements at the right hand of pivot
    doQuickSort(array, pivotIndex + 1, endIndex)
  }

  doQuickSort(array, 0, array.length - 1)
}


val array = Array(7, 6, 5, 4, 3, 2, 1)
quickSort(array)

println(
  array.sameElements(Array(1, 2, 3, 4, 5, 6, 7))
)