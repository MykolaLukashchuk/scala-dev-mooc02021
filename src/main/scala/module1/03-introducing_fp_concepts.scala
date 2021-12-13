package module1

import scala.annotation.tailrec
import scala.language.postfixOps


/**
 *  Реализуем тип Option
 */

object opt {

  /**
   *
   * Реализовать тип Option, который будет указывать на присутствие либо отсутсвие результата
   */

  sealed trait Option[+T]{
    def isEmpty: Boolean = this match {
      case Option.Some(v) => false
      case Option.None => true
    }

    def get: T = this match {
      case Option.Some(v) => v
      case Option.None => throw new Exception("Get on empty option")
    }

    def map[B](f: T => B): Option[B] = this match {
      case Option.Some(v) => Option.Some(f(v))
      case Option.None => Option.None
    }

    def flatMap[B](f: T => Option[B]): Option[B] = this match {
      case Option.Some(v) => f(v)
      case Option.None => Option.None
    }

    /**
     *
     * Реализовать метод printIfAny, который будет печатать значение, если оно есть
     */
    def printIfAny(): Any = this match {
      case Option.Some(v) => print(v)
      case Option.None =>
    }

    /**
     *
     * Реализовать метод zip, который будет создавать Option от пары значений из 2-х Option
     */
    def zip[B](o: Option[B]): Option[(T, B)] = {
      this match {
        case Option.Some(first) => o match {
          case Option.Some(second) => Option.Some(first, second)
          case Option.None => Option.None
        }
        case Option.None => Option.None
      }
    }

    /**
     *
     * Реализовать метод filter, который будет возвращать не пустой Option
     * в случае если исходный не пуст и предикат от значения = true
     */
    def filter(f: T => Boolean): Option[T] = this match {
      case Option.Some(v) => if (f(v)) this else Option.None
      case Option.None => Option.None
    }
  }

  object Option{
    case class Some[T](v: T) extends Option[T]
    case object None extends Option[Nothing]

    def apply[T](v: T): Option[T] = if (v == null) Some(v) else None
  }

}

object List {
  /**
   *
   * Реализовать односвязанный иммутабельный список List
   * Список имеет два случая:
   * Nil - пустой список
   * Cons - непустой, содердит первый элемент (голову) и хвост (оставшийся список)
   */

  sealed trait List[+T] {
    /**
     * Метод cons, добавляет элемент в голову списка, для этого метода можно воспользоваться названием `::`
     *
     */
    //     def cons[B <: T](head: B): List[T] = this match {
    //       case ::(_, _) => list.::(head, this)
    //       case Nil => list.::(head, Nil)
    //     }

    //     def ::(head: T): List[T] = List ::(head, this)

    /**
     * Метод mkString возвращает строковое представление списка, с учетом переданного разделителя
     *
     */
    def mkString[A](separator: A): String = {
      @tailrec
      def loop(result: String, list: List[T]): String = list match {
        case ::(head, tail) => loop(s"$result$separator$head", tail)
        case Nil => result
      }

      this match {
        case ::(head, tail) => loop(s"$head", tail)
        case Nil => "Empty list"
      }
    }

    /**
     *
     * Реализовать метод reverse который позволит заменить порядок элементов в списке на противоположный
     */

    def reverse(): List[T] = {
      @tailrec
      def loop(target: List[T], result: List[T]): List[T] = target match {
        case ::(head, tail) => loop(tail, ::(head, result))
        case Nil => result
      }

      this match {
        case ::(head, tail) => loop(tail, ::(head, List.Nil))
        case Nil => Nil
      }
    }


    /**
     *
     * Реализовать метод map для списка который будет применять некую ф-цию к элементам данного списка
     */
    def map[B](f: T => B): List[B] = {
      @tailrec
      def loop(target: List[T], result: List[B]): List[B] = target match {
        case ::(head, tail) => loop(tail, ::(f(head), result))
        case Nil => result
      }

      this match {
        case ::(head, tail) => loop(tail, ::(f(head), Nil))
        case Nil => Nil
      }
    }

    /**
     *
     * Реализовать метод filter для списка который будет фильтровать список по некому условию
     */
    def filter(f: T => Boolean): List[T] = {
      @tailrec
      def loop(target: List[T], result: List[T]): List[T] = target match {
        case ::(head, tail) => loop(tail, if (f(head)) ::(head, result) else result)
        case Nil => result
      }

      this match {
        case ::(head, tail) => loop(tail, if (f(head)) ::(head, Nil) else Nil)
        case Nil => Nil
      }
    }

  }

  case class ::[A](head: A, tail: List[A]) extends List[A]
  case object Nil extends List[Nothing]

  /**
   * Конструктор, позволяющий создать список из N - го числа аргументов
   * Для этого можно воспользоваться *
   *
   * Например вот этот метод принимает некую последовательность аргументов с типом Int и выводит их на печать
   * def printArgs(args: Int*) = args.foreach(println(_))
   */
  def apply[A](values: A*): List[A] = {
    var result: List[A] = List.Nil
    for (value <- values) {
      result = List.::(value, result)
    }
    result
  }

  /**
   *
   * Написать функцию incList котрая будет принимать список Int и возвращать список,
   * где каждый элемент будет увеличен на 1
   */
  def incList(target: List[Int]): List[Int] = {
    @tailrec
    def loop(target: List[Int], result: List[Int]): List[Int] = target match {
      case ::(head, tail) => loop(tail, ::(head + 1, result))
      case Nil => result
    }

    target match {
      case ::(head, tail) => loop(tail, ::(head + 1, Nil))
      case Nil => Nil
    }
  }

  /**
   *
   * Написать функцию shoutString котрая будет принимать список String и возвращать список,
   * где к каждому элементу будет добавлен префикс в виде '!'
   */

  def shoutString(target: List[String]): List[String] = {
    val Prefix = '!'
    @tailrec
    def loop(target: List[String], result: List[String]): List[String] = target match {
      case ::(head, tail) => loop(tail, ::(s"$Prefix$head", result))
      case Nil => result
    }

    target match {
      case ::(head, tail) => loop(tail, ::(s"$Prefix$head", Nil))
      case Nil => Nil
    }
  }
}