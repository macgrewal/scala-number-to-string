package services

object NumberToStringService {

  implicit class Ones(val x: Int) {
    override def toString: String = basicNumString(x)
    def isEmpty: Boolean = x == 0
  }

  implicit class Tens(val x: Int) {
    val ones: Option[Ones] = None

    def ty(newOnes: Ones): Tens = {
      if(newOnes.isEmpty)
        this
      else
        new Tens(x){override val ones = Some(newOnes)}
    }

    override def toString: String = {
      if(x < 2)
        basicNumString(x * 10 + ones.map(_.x).getOrElse(0))
      else
        ones.map { definedOnes =>
          s"${tensString(x)} $definedOnes"
        }.getOrElse(tensString(x))
    }

    def isEmpty: Boolean = {
      x == 0 && ones.isEmpty
    }
  }

  implicit class Hundreds(x: Int) {
    val tens: Option[Tens] = None

    def hundredAnd(newTens: Tens): Hundreds = {
      if(newTens.isEmpty)
        this
      else
        new Hundreds(x){override val tens = Some(newTens)}
    }

    override def toString: String = {
      (x, tens) match {
        case (0, None)                => ""
        case (0, Some(definedTens))   => s"$definedTens"
        case (num, None)              => s"${basicNumString(num)} hundred"
        case (num, Some(definedTens)) => s"${basicNumString(num)} hundred and $definedTens"
      }
    }

    def hasHundredValue: Boolean = {
      x != 0
    }

    def isEmpty: Boolean = {
      x == 0 && tens.isEmpty
    }
  }

  implicit class Thousands(x: Int) {
    val hundreds: Option[Hundreds] = None

    def thousand(newHundreds: Hundreds): Thousands = {
      if(newHundreds.isEmpty)
        this
      else
        new Thousands(x){override val hundreds = Some(newHundreds)}
    }

    override def toString: String = {
      (x, hundreds) match {
        case (0, None)                    => "zero"
        case (0, Some(definedHundreds))   => s"$definedHundreds"
        case (num, None)                  => s"${basicNumString(num)} thousand"
        case (num, Some(definedHundreds)) if definedHundreds.hasHundredValue =>
          s"${basicNumString(num)} thousand $definedHundreds"
        case (num, Some(definedHundreds)) =>
          s"${basicNumString(num)} thousand and $definedHundreds"
      }
    }
  }

  private def getDigits(x: Int): (Int, Int, Int, Int) = {
    val str = "0000".concat(x.toString).takeRight(4)
    val index = str.length - 4
    (charToInt(str(index)), charToInt(str(index+1)), charToInt(str(index+2)), charToInt(str(index+3)))
  }

  private def charToInt(c: Char): Int = c.toString.toInt

  private def basicNumString(n: Int): String = {
    n match {
      case 0 => ""
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
      case num => throw new RuntimeException(s"$num was more than 19")
    }
  }

  private def tensString(n: Int): String = {
    n match {
      case 2 => "twenty"
      case 3 => "thirty"
      case 4 => "forty"
      case 5 => "fifty"
      case 6 => "sixty"
      case 7 => "seventy"
      case 8 => "eighty"
      case 9 => "ninety"
      case num => throw new RuntimeException(s"$num was invalid for tens")
    }
  }

  def numberToString(num: Int): String = {
    if(num < 0 || num > 9999) {
      "out of range"
    } else {
      val (a, b, c, d) = getDigits(num)
      (a thousand (b hundredAnd (c ty d))).toString
    }
  }

}
