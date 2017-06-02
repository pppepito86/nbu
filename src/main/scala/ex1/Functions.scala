package ex1

import scala.annotation.tailrec

object Functions {

  // Връща големината на масив (без да ползва data.length!!!)
  def length(data: List[Int]): Int = data match {
    case Nil => 0
    case _::xs => 1 + length(xs)
  }

  // Ако cond е true връща onTrue
  def ifelse(cond: Boolean, onTrue: Int, onFalse: Int): Int = if (cond) onTrue else onFalse

  // Проверява дали скобите в даден масив от символи са балансирани.
  // Коректно: (a)asda(b)(v) | (((a))) | ()(()асдасд)
  // Грешно: )() | ((д) | ((das) (d)(
  def balance(chars: List[Char]): Boolean = {
    def balance(c: List[Char], opened: Int): Boolean = c match {
      case Nil => opened == 0
      case '('::xs => balance(xs, opened+1)
      case ')'::xs => opened > 0 && balance(xs, opened - 1)
      case _::xs => balance(xs, opened)
    }

    balance(chars, 0)
  }

  def map(chars: List[Char], f: Char => Char): List[Char] = chars map (x => f(x))

  def toUpperCase(chars: List[Char]): List[Char] = {
    def upperCase(char: Char): Char = char.toUpper

    map(chars, upperCase)
  }

  // Проверява дали съществува елемент отговарящ на f
  def exists(data: List[Int], f: Int => Boolean): Boolean = data match {
    case Nil => false
    case x::xs => f(x)||exists(xs, f)
  }

  // Връща масив съдържащ само елементите отговарящи на f
  def filter(data: List[Int], f: Int => Boolean): List[Int] = data match {
    case Nil => List()
    case x::xs => if (f(x)) x::filter(xs, f) else filter(xs, f)
  }

  // Проверява дали всички елементи отговарят на f
  def forall(data: List[Int], f: Int => Boolean): Boolean = data match {
    case Nil => true
    case x::xs => f(x)&&forall(xs, f)
  }

  // Връща числото от триъгълника на Паскал отговарящо на съответния ред/колона
  def pascal(c: Int, r: Int): Int = r match {
    case x if x < 1 || x > c => 0
    case 1 if c == 1 => 1
    case x => pascal(c-1, x-1) + pascal(c-1, x)
  }
}