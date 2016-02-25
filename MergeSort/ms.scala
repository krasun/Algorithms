// https://en.wikipedia.org/wiki/Merge_sort

def mergeSort(array: Array[Int]): Unit = {

  def merge(array: Array[Int], startIndex: Int, middleIndex: Int, endIndex: Int): Unit = {
    val leftArray = array.slice(startIndex, middleIndex + 1) ++ Array(Double.PositiveInfinity.toInt)
    val rightArray = array.slice(middleIndex + 1, endIndex + 1) ++ Array(Double.PositiveInfinity.toInt)

    var leftIndex, rightIndex = 0
    for (arrayIndex <- startIndex to endIndex) {
      if (leftArray(leftIndex) <= rightArray(rightIndex)) {
        array(arrayIndex) = leftArray(leftIndex)
        leftIndex += 1
      } else {
        array(arrayIndex) = rightArray(rightIndex)
        rightIndex += 1
      }
    }
  }

  def doMergeSort(array: Array[Int], startIndex: Int, endIndex: Int): Unit = {
    if (startIndex >= endIndex) {
      return
    }

    val middleIndex = math.floor((startIndex + endIndex) / 2).toInt

    doMergeSort(array, startIndex, middleIndex)
    doMergeSort(array, middleIndex + 1, endIndex)

    merge(array, startIndex, middleIndex, endIndex)
  }

  doMergeSort(array, 0, array.length - 1)
}


val array = Array(7, 6, 5, 4, 3, 2, 1)
mergeSort(array)

println(
  array.sameElements(Array(1, 2, 3, 4, 5, 6, 7))
)