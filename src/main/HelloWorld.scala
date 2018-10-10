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

object HelloWorld {

  implicit class Tap[T](self: T) {
    def tap[U](block: T => U): T = {
      block(self) //値は捨てる
      self
    }
  }

  implicit val b: Int = (1 to 5).foldLeft(0)((a, b) => a + b)

// Error ambiguous implicit values:
// implicit val c: Int = (1 to 2).foldLeft(0)((a, b) => a + b)

  def hoge(a: Int)(implicit b: Int): Unit = {
    println(b + a)
  }

  def main(args: Array[String]): Unit = {
    val arr = Array(1, 2, 3, 4, 5)
    User.swapArray(arr)(1, 2)
    println(joinByComma(1, 5))
    println((1 to 5).foldLeft(0)((b, a) => b + a))

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
