package services

import scala.annotation.tailrec

object NumberToStringService {

  val units: Int => String = {
    case 1 => "one"
    case 2 => "two"
    case 3 => "three"
    case 4 => "four"
    case 5 => "five"
    case 6 => "six"
    case 7 => "seven"
    case 8 => "eight"
    case 9 => "nine"
    case 10 => "ten"
    case 11 => "eleven"
    case 12 => "twelve"
    case 13 => "thirteen"
    case 14 => "fourteen"
    case 15 => "fifteen"
    case 16 => "sixteen"
    case 17 => "seventeen"
    case 18 => "eighteen"
    case 19 => "nineteen"
    case _ => ""
  }

  val tens: Int => String = {
    case 20 => "twenty"
    case 30 => "thirty"
    case 40 => "forty"
    case 50 => "fifty"
    case 60 => "sixty"
    case 70 => "seventy"
    case 80 => "eighty"
    case 90 => "ninety"
    case number =>
      val ten = number / 10 * 10
      val other = number - ten
      s"${tens(ten)} ${units(other)}"
  }

  val hundreds: Int => (Int, String) = number => {
    val hundred = number / 100
    val remainder = number - hundred * 100
    (remainder, units(hundred) + " hundred")
  }

  val thousands: Int => (Int, String) = number => {
    val thousand = number / 1000
    val remainder = number - thousand * 1000
    (remainder, units(thousand) + " thousand")
  }

  def numberToString(num: Int): String = {

    val and = if (num > 99) "and " else ""

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
        val word = tens(number)
        buildNumber(0, s"$acc $and$word")
      case _ =>
        val word = units(number)
        buildNumber(0, s"$acc $and$word")
    }

    num match {
      case n if n < 0 => "out of range"
      case n if n >= 10000 => "out of range"
      case 0 => "zero"
      case n => buildNumber(n).trim
    }
  }

}
