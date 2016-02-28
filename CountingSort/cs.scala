// https://en.wikipedia.org/wiki/Counting_sort

def countingSort(array: Array[Int], numbersVolume: Int): Array[Int] = {

  def countKeysEqual(array: Array[Int], numbersVolume: Int): Array[Int] = {
    val equal = new Array[Int](numbersVolume)
    for (index <- array.indices) {
      equal(array(index)) += 1
    }

    equal
  }

  def countKeysLess(equal: Array[Int]): Array[Int] = {
    var less = new Array[Int](equal.length)
    for (index <- 1 until equal.length) {
      less(index) += less(index - 1) + equal(index - 1)
    }

    less
  }

  def rearrange(array: Array[Int], less: Array[Int]): Array[Int] = {
    val sortedArray = new Array[Int](array.length)
    val next = new Array[Int](less.length)

    for (index <- less.indices) {
      next(index) = less(index)
    }

    for (index <- array.indices) {
      val key = array(index)
      val nextIndex = next(key)

      sortedArray(nextIndex) = array(index)

      next(key) += 1
    }

    sortedArray
  }

  val equal = countKeysEqual(array, numbersVolume)
  val less = countKeysLess(equal)

  rearrange(array, less)
}

val array = Array(7, 6, 5, 4, 3, 2, 1)
val sortedArray = countingSort(array, 8)

println(
  sortedArray.sameElements(Array(1, 2, 3, 4, 5, 6, 7))
)