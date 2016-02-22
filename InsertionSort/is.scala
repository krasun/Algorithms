// https://en.wikipedia.org/wiki/Selection_sort

def selectionSort(array: Array[Int]): Unit = {
  for (index <- 0 to (array.length - 2)) {
    var smallestIndex = index

    // find smallest index
    for (restIndex <- (index + 1) to (array.length - 1)) {
      if (array(restIndex) < array(smallestIndex)) {
        smallestIndex = restIndex
      }
    }

    // exchange current element with smallest
    val temporary = array(index)
    array(index) = array(smallestIndex)
    array(smallestIndex) = temporary
  }
}

val array = Array(5, 4, 3, 2, 1)
selectionSort(array)

println(
  array.sameElements(Array(1, 2, 3, 4, 5))
)
