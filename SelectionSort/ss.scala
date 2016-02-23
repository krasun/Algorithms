// https://en.wikipedia.org/wiki/Insertion_sort

def insertionSort(array: Array[Int]): Unit = {
  for (index <- 1 to (array.length - 1)) {
    val key = array(index)
    var j = index - 1
    while (j > 0 && array(j) > key) {
      array(j + 1) = array(j)
      j = j - 1
    }
    array(j + 1) = key
  }
}

val array = Array(5, 4, 3, 2, 1)
insertionSort(array)

println(
  array.sameElements(Array(1, 2, 3, 4, 5))
)
