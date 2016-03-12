// https://en.wikipedia.org/wiki/Factorial

def factorial(n: Int): Int = {
  def loop(n: Int, acc: Int): Int = {
    if (n <= 0) acc else loop(n - 1, acc * n)
  }

  loop(n, 1)
}

println(1 == factorial(0))
println(1 == factorial(1))
println(2 == factorial(2))
println(6 == factorial(3))
