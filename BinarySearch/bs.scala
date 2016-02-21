// https://en.wikipedia.org/wiki/Binary_search_algorithm

def binarySearch(haystack: Array[Int], needle: Int, minIndex: Int, maxIndex: Int): Option[Int] = {
  if (minIndex > maxIndex) {
    return None
  }

  val middleIndex = math.floor((minIndex + maxIndex) / 2).toInt
  if (haystack(middleIndex) == needle) {
    return new Some(middleIndex)
  }

  if (haystack(middleIndex) > needle) {
    binarySearch(haystack, needle, minIndex, middleIndex - 1)
  } else {
    binarySearch(haystack, needle, middleIndex + 1, maxIndex)
  }
}

def search(haystack: Array[Int], needle: Int): Option[Int] = binarySearch(haystack, needle, 0, haystack.length - 1)

val haystack = Array[Int](0, 10, 20, 30, 40, 50, 60)

println(
  true
    && search(haystack, 0).get == 0
    && search(haystack, 20).get == 2
    && search(haystack, 60).get == 6
)