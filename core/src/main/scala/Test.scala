object Helper{
	//print test seperator
	def pts (text: String, seperator: String, length: Int){
		println(seperator * length)
		println(text)
	}
	//standard test seperator
	def spts(text: String) = pts(text,"=",75)
}

object Test extends App {
  import Helper.spts
	spts("hello")

  import ConvertMe._

  // Example of expanded variants, based on:
  //
  // https://github.com/yfcai/cata/blob/master/list3.scala

  class Nil[T, L]() extends ListsF[T, L] {
    def map[R](f: L => R): ListsF[T, R] = new Nil()
  }

  // beware the parameter list: they are `val` definitions.
  class Cons[T, L](val head: T, val tail: L) extends ListsF[T, L] {
    def map[R](f: L => R): ListsF[T, R] = new Cons(head, f(tail))
  }

  object Nil {
    private class FixedNil[T]() extends Nil[T, Lists[T]] with Lists[T]

    def apply[T](): Lists[T] = new FixedNil()

    // beware the return type: when the argument list is empty,
    // then the return type of `unapply` is Boolean, not Option[()].
    def unapply[T, L](nil: ListsF[T, L]): Boolean =
      nil.isInstanceOf[Nil[T, L]]
  }

  object Cons {
    private class FixedCons[T](head: T, tail: Lists[T])
        extends Cons[T, Lists[T]](head, tail) with Lists[T]

    def apply[T](head: T, tail: Lists[T]): Lists[T] =
      new FixedCons(head, tail)

    def unapply[T, L](cons: ListsF[T, L]): Option[(T, L)] = cons match {
      case cons: Cons[T, L] => Some((cons.head, cons.tail))
      case _ => None
    }
  }


  // Tests

  val xs = Cons(1, Cons(2, Cons(3, Cons(4, Nil()))))

  def sum1(xs: Lists[Int]): Int = xs.fold[Int] {
    case Nil() => 0
    case Cons(head, tail) => head + tail
  }

  def sum2(xs: Lists[Int]): Int = xs match {
    case Nil() => 0
    case Cons(head, tail) => head + sum2(tail)
  }

  println(s"sum1 = ${sum1(xs)}")
  println(s"sum2 = ${sum2(xs)}")
}

@convert
object ConvertMe{
  trait Lists[T]

  // a case class with a name as argument is always bound
  case class Nil[T] extends Lists[T]
  case class Cons[T](head:T, tail:Lists[T]) extends Lists[T]

}


