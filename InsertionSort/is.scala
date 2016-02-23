// https://en.wikipedia.org/wiki/Insertion_sort

def insertionSort(array: Array[Int]): Unit = {
  for (index <- 1 to (array.length - 1)) {
    val key = array(index)
    var searchIndex = index - 1

    // move elements greater than key and find new position index for key
    while (searchIndex > -1 && array(searchIndex) > key) {
      array(searchIndex + 1) = array(searchIndex)
      searchIndex = searchIndex - 1
    }

    array(searchIndex + 1) = key
  }
}

val array = Array(5, 4, 3, 2, 1)
insertionSort(array)

println(
  array.sameElements(Array(1, 2, 3, 4, 5))
)
