import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.language.postfixOps
import scala.util.{Failure, Success, Random}

sealed abstract class DayOfWeek
case object Sunday extends DayOfWeek
case object Monday extends DayOfWeek
case object Tuesday extends DayOfWeek
case object Wednesday extends DayOfWeek
case object Thursday extends DayOfWeek
case object Friday extends DayOfWeek
case object Saturday extends DayOfWeek

sealed abstract class Tree
case class Branch(value: Int, left: Tree, right: Tree) extends Tree
case object Empty extends Tree

trait Additive[A] {
  def plus(a: A, b: A): A
  def zero: A
}

object Serializers {
  trait Serializer[A] {
    def serialize(obj: A): String
  }
  def string[A: Serializer](obj: A): String = {
    implicitly[Serializer[A]].serialize(obj)
  }
}

object FromInts {
  trait FromInt[A] {
    def to(from: Int): A
  }
  object FromInt {
    implicit object FromIntToInt extends FromInt[Int] {
      def to(from: Int): Int = from
    }
    implicit object FromIntToDouble extends FromInt[Double] {
      def to(from: Int): Double = from
    }
  }
}

object Nums {
  trait Num[A] {
    def plus(a: A, b: A): A
    def minus(a: A, b: A): A
    def multiply(a: A, b: A): A
    def divide(a: A, b: A): A
    def zero: A
  }
  object Num {
    implicit object IntNum extends Num[Int] {
      def plus(a: Int, b: Int): Int = a + b
      def minus(a: Int, b: Int): Int = a - b
      def multiply(a: Int, b: Int): Int = a * b
      def divide(a: Int, b: Int): Int = a / b
      def zero: Int = 0
    }
    implicit object DoubleNum extends Num[Double] {
      def plus(a: Double, b: Double): Double = a + b
      def minus(a: Double, b: Double): Double = a - b
      def multiply(a: Double, b: Double): Double = a * b
      def divide(a: Double, b: Double): Double = a / b
      def zero: Double = 0.0
    }
  }
}

object HelloWorld {
  import FromInts._
  import Nums._
  import Serializers._
  def sum[A](lst: List[A])(implicit m: Additive[A]) =
    lst.foldLeft(m.zero)((x, y) => m.plus(x, y))

  //  def average[A](lst: List[A])(implicit a: Num[A], b: FromInt[A]): A = {
  def average[A: Num: FromInt](lst: List[A]): A = {
    val a = implicitly[Num[A]]
    val b = implicitly[FromInt[A]]
    val length: Int = lst.length
    val sum: A = lst.foldLeft(a.zero)((x, y) => a.plus(x, y))
    a.divide(sum, b.to(length))
  }

  def median[A: Num: Ordering: FromInt](lst: List[A]): A = {
    val num = implicitly[Num[A]]
    // val ord = implicitly[Ordering[A]]
    val int = implicitly[FromInt[A]]
    val size = lst.size
    require(size > 0)
    val sorted = lst.sorted
    if (size % 2 == 1) {
      sorted(size / 2)
    } else {
      val fst = sorted((size / 2) - 1)
      val snd = sorted((size / 2))
      num.divide(num.plus(fst, snd), int.to(2))
    }
  }

  implicit class Tap[T](self: T) {
    def tap[U](block: T => U): T = {
      block(self) //値は捨てる
      self
    }
  }

  implicit val b: Int = (1 to 5).foldLeft(0)((a, b) => a + b)
  implicit object StringAdditive extends Additive[String] {
    def plus(a: String, b: String): String = a + b
    def zero: String = ""
  }

  implicit object IntSerializer extends Serializer[Int] {
    def serialize(obj: Int): String = obj.toString
  }
  implicit object StringSerializer extends Serializer[String] {
    def serialize(obj: String): String = obj
  }

  implicit object IntAdditive extends Additive[Int] {
    def plus(a: Int, b: Int): Int = a + b
    def zero: Int = 0
  }

// Error ambiguous implicit values:
// implicit val c: Int = (1 to 2).foldLeft(0)((a, b) => a + b)

  def hoge(a: Int)(implicit b: Int): Unit = {
    println(b + a)
  }

  def main(args: Array[String]): Unit = {
    val arr = Array(1, 2, 3, 4, 5)
    User.swapArray(arr)(1, 2)
    println(joinByComma(1, 5))
    println((1 to 5).foldLeft(0)(_ + _))

    val tree = Branch(9,
                      Branch(2, Empty, Empty),
                      Branch(3,
                             Branch(7, Empty, Branch(23, Empty, Empty)),
                             Branch(17, Empty, Empty)))
    println(Tree.max(tree))
    println(Tree.min(tree))
    println(Tree.depth(Empty))
    println(Tree.depth(Branch(10, Empty, Empty)))
    println(
      Tree.depth(
        Branch(10,
               Branch(20, Empty, Empty),
               Branch(30, Branch(40, Empty, Empty), Empty))))
    println(Tree.sort(tree))

    "Hello, World"
      .tap { s =>
        println(s)
      }
      .reverse
      .tap { s =>
        println(s)
      }

    11111
      .tap { s =>
        println(s)
      }

    hoge(10)
    println(sum(List(1, 2, 3)))
    println(sum(List("A", "B")))

    assert(2.5 == median(List(1.5, 2.5, 3.5)))

    val s = "Hello"
    val f: Future[String] = Future {
      Thread.sleep(1000)
      s + " future!"
    }

    f.foreach {
      case s: String =>
        println(s)
    }

    println(f.isCompleted) // false

    // f.wait()

    Thread.sleep(5000) // Hello future!

    println(f.isCompleted) // true

    val random = new Random()
    val waitMaxMilliSec = 3000

    def waitRandom(futureName: String): Int = {
      val waitMilliSec = random.nextInt(waitMaxMilliSec);
      if (waitMilliSec < 500)
        throw new RuntimeException(
          s"${futureName} waitMilliSec is ${waitMilliSec}")
      Thread.sleep(waitMilliSec)
      waitMilliSec
    }

    val futureFirst: Future[Int] = Future { waitRandom("first") }
    val futureSecond: Future[Int] = Future { waitRandom("second") }

    val compositeFuture: Future[(Int, Int)] = for {
      first <- futureFirst
      second <- futureSecond
    } yield (first, second)

    compositeFuture onComplete {
      case Success((first, second)) =>
        println(s"Success! first:${first} second:${second}")
      case Failure(t) => println(s"Failure: ${t.getMessage}")
    }

    Thread.sleep(5000)
  }

  def joinByComma(start: Int, end: Int): String = {
    (start to end).mkString(",")
  }

  def nextDayOfWeek(d: DayOfWeek): DayOfWeek = d match {
    case Sunday    => Monday
    case Monday    => Tuesday
    case Tuesday   => Wednesday
    case Wednesday => Thursday
    case Thursday  => Friday
    case Friday    => Saturday
    case Saturday  => Sunday
  }
}

object Tree {
  def max(tree: Tree): Int = {
    def _max(tree: Tree, v: Int): Int = {
      tree match {
        case Branch(a, l, r) => {
          val b = if (a > v) a else v
          List(b, _max(l, b), _max(r, b)).max
        }
        case Empty => v
      }
    }
    _max(tree, 0)
  }

  def min(tree: Tree): Int = {
    def _min(tree: Tree, v: Int): Int = {
      tree match {
        case Branch(a, l, r) => {
          val b = if (a < v) a else v
          List(b, _min(l, b), _min(r, b)).min
        }
        case Empty => v
      }
    }
    _min(tree, Int.MaxValue)
  }

  def toList(tree: Tree): List[Int] = tree match {
    case Empty           => Nil
    case Branch(v, l, r) => toList(l) ++ List(v) ++ toList(r)
  }

  def depth(tree: Tree): Int = {
    def _depth(tree: Tree, v: Int): Int = {
      tree match {
        case Branch(a, l, r) => {
          List(_depth(l, v + 1), _depth(r, v + 1)).max
        }
        case Empty => v
      }
    }
    _depth(tree, 0)
  }

  def sort(t: Tree): Tree = {
    def fromList(list: List[Int]): Tree = {
      def insert(value: Int, t: Tree): Tree = t match {
        case Empty => Branch(value, Empty, Empty)
        case Branch(v, l, r) =>
          if (value <= v) Branch(v, insert(value, l), r)
          else Branch(v, l, insert(value, r))
      }
      list.foldLeft(Empty: Tree) { case (t, v) => insert(v, t) }
    }
    val a = toList(t)
    println(a.mkString(" , "))
    fromList(a)
  }
}

class User(val name: String, val age: Int)

object User {
  def printUser(user: User) = println(user.name + " " + user.age)
  def swapArray[T](arr: Array[T])(i: Int, j: Int): Unit = {
    val tmp = arr(i)
    arr(i) = arr(j)
    arr(j) = tmp
  }
}
