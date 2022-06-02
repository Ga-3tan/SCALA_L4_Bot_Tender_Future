package Utils

import scala.annotation.tailrec

/**
  * Contains the function necessary to calculate the number of *clinks* when n people want to cheers.
  */
object ClinksCalculator:
  /**
    * Calculate the factorial of a given number
    * @param n the number to compute
    * @return n!
    */
  def factorial(n: Int): BigInt = {
    @tailrec
    def loop(acc: BigInt, n: Int): BigInt = {
      if n <= 0 then acc
      else loop(acc*n, n-1)
    }
    loop(1 , n)
  }

  /**
    * Calculate the combination of two given numbers
    * @param n the first number
    * @param k the second number
    * @return n choose k
    */
  def calculateCombination(n: Int, k: Int): Int = {
    if k > n then 0
    else (factorial(n)/factorial(k)/factorial(n-k)).toInt
  }
end ClinksCalculator
