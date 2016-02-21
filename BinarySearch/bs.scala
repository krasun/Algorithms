// https://en.wikipedia.org/wiki/Binary_search_algorithm

def binarySearch(a: Array[Int], p: Int, r: Int, x: Int): Option[Int] = {
  if (p > r) {
    return None
  }

  val q = (p + r) / 2
  if (a(q) == x) {
    return new Some(q)
  }

  if (a(q) > x) {
    binarySearch(a, p, q - 1, x)
  } else {
    binarySearch(a, q + 1, r, x)
  }
}

def search(a: Array[Int], x: Int): Option[Int] = binarySearch(a, 0, a.length - 1, x)

val a = Array[Int](0, 10, 20, 30, 40, 50, 60)

println(
  true
    && search(a, 0).get == 0
    && search(a, 20).get == 2
    && search(a, 60).get == 6
)