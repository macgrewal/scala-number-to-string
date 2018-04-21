package services

import scala.annotation.tailrec

object NumberToStringService {

  val units: Map[Int, String] = Map(
    1 -> "one",
    2 -> "two",
    3 -> "three",
    4 -> "four",
    5 -> "five",
    6 -> "six",
    7 -> "seven",
    8 -> "eight",
    9 -> "nine",
    10 -> "ten",
    11 -> "eleven",
    12 -> "twelve",
    13 -> "thirteen",
    14 -> "fourteen",
    15 -> "fifteen",
    16 -> "sixteen",
    17 -> "seventeen",
    18 -> "eighteen",
    19 -> "nineteen"
  )

  val tenWord: Map[Int, String] = Map(
    20 -> "twenty",
    30 -> "thirty",
    40 -> "forty",
    50 -> "fifty",
    60 -> "sixty",
    70 -> "seventy",
    80 -> "eighty",
    90 -> "ninety"
  )

  val tens: Int => (Int, String) = number => {
    val ten = number / 10 * 10
    val remainder = number - ten
    (remainder, tenWord(ten))
  }

  val hundreds: Int => (Int, String) = number => {
    val hundred = number / 100
    val remainder = number - hundred * 100
    val suffix = if (remainder > 0) "and" else ""
    (remainder, (units(hundred) + s" hundred $suffix").trim)
  }

  val thousands: Int => (Int, String) = number => {
    val thousand = number / 1000
    val remainder = number - thousand * 1000
    val suffix = if (remainder > 0 && remainder < 100) "and" else ""
    (remainder, (units(thousand) + s" thousand $suffix").trim)
  }

  def numberToString(num: Int): String = {

    @tailrec
    def buildNumber(number: Int, acc: String = ""): String = number match {
      case 0 => acc
      case n if n > 999 =>
        val (remainder, word) = thousands(number)
        buildNumber(remainder, s"$acc $word")
      case n if n > 99 =>
        val (remainder, word) = hundreds(number)
        buildNumber(remainder, s"$acc $word")
      case n if n > 19 =>
        val (remainder, word) = tens(number)
        buildNumber(remainder, s"$acc $word")
      case _ =>
        val word = units(number)
        buildNumber(0, s"$acc $word")
    }

    num match {
      case n if n < 0 => "out of range"
      case n if n >= 10000 => "out of range"
      case 0 => "zero"
      case n => buildNumber(n).trim
    }
  }

}
