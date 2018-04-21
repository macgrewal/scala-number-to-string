package services

import scala.annotation.tailrec

object NumberToStringService {

  type wordToNumberFunc = Int => (Int, String)

  val numberToWord: Map[Int, String] = Map(
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
    19 -> "nineteen",
    20 -> "twenty",
    30 -> "thirty",
    40 -> "forty",
    50 -> "fifty",
    60 -> "sixty",
    70 -> "seventy",
    80 -> "eighty",
    90 -> "ninety"
  )

  def numberToString(num: Int): String = {

    val tens: wordToNumberFunc = number => {
      val ten = number / 10 * 10
      val remainder = number - ten
      (remainder, numberToWord(ten))
    }

    val hundreds: wordToNumberFunc = number => {
      val hundred = number / 100
      val remainder = number - hundred * 100
      val suffix = if (remainder > 0) "and" else ""
      (remainder, (numberToWord(hundred) + s" hundred $suffix").trim)
    }

    val thousands: wordToNumberFunc = number => {
      val thousand = number / 1000
      val remainder = number - thousand * 1000
      val suffix = if (remainder > 0 && remainder < 100) "and" else ""
      (remainder, (numberToWord(thousand) + s" thousand $suffix").trim)
    }

    val buildersWithGuards: Seq[(Int, wordToNumberFunc)] = Seq(
      999 -> thousands,
      99 -> hundreds,
      19 -> tens
    )

    @tailrec
    def buildNumber(number: Int,
                    wordBuilders: Seq[(Int, wordToNumberFunc)],
                    acc: String = ""): String = {

      (number, wordBuilders) match {
        case (0, _) => acc
        case (n, _) if n < 20 =>
          val word = numberToWord(n)
          buildNumber(0, Seq.empty, s"$acc $word")
        case (n, (min, function) :: functions) if n > min =>
          val (remainder, word) = function(n)
          buildNumber(remainder, functions, s"$acc $word")
        case _ =>
          buildNumber(number, wordBuilders.tail, acc)
      }
    }

    num match {
      case n if n < 0 => "out of range"
      case n if n >= 10000 => "out of range"
      case 0 => "zero"
      case _ => buildNumber(num, buildersWithGuards).trim
    }
  }

}
