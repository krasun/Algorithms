// https://en.wikipedia.org/wiki/Fibonacci_number

def fib(n: Int): Int = {

  @annotation.tailrec
  def loop(n: Int, f1: Int, f2: Int): Int = {
    if (n <= 0) {
      f1
    } else {
      loop(n - 1, f2, f1 + f2)
    }
  }

  loop(n, 0, 1)
}

println(fib(4))
println(fib(4) == 3)

println(fib(5))
println(fib(5) == 5)

