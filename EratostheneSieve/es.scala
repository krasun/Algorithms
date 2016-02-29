// https://en.wikipedia.org/wiki/Sieve_of_Eratosthenes

def computePrimes(limit: Int): Seq[Int] = {
  val sieve = new Array[Boolean](limit + 1)
  for (number <- 2 to limit) {
    if (! sieve(number)) {
      for (number <- number * number to limit by number) {
        sieve(number) = true
      }
    }
  }

  for (number <- sieve.indices; if number >= 2 && !sieve(number)) yield number
}

val primes = computePrimes(30)

println(primes.mkString(" "))
println(primes.equals(Array(2, 3, 5, 7, 11, 13, 17, 19, 23, 29).toSeq))

