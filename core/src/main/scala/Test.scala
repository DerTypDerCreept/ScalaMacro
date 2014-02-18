object Helper{
    //print test seperator
    def pts (text: String, seperator: String, length: Int){
        println(seperator * length)
        println(text)
    }
    //standard test seperator
    def spts(text: String) = pts(text,"=",75)
}





object Test extends App with ConvertMe{
    import Helper.spts
    //import ConvertMe._
    spts("hello")
    //ConvertMe.saySomething
    val xs = Cons(1, Cons(2, Cons(3, Cons(4, Nil()))))
      def sum1(xs: Lists[Int]): Int = xs.fold[Int] {
        case Nil() => 0
        case Cons(head, tail) => head  +   tail
      }
      def sum2(xs: Lists[Int]): Int = xs match {
        case Nil() => 0
        case Cons(head, tail) => head  +   sum2(tail)
      }
      println(s"sum1 = ${sum1(xs)}")
      println(s"sum2 = ${sum2(xs)}")
    }

    
@convert
trait ConvertMe{
  trait Lists[T]

  // a case class with a name as argument is always bound
  case class Nil[T] extends Lists[T]
  case class Cons[T](head:T, tail:Lists[T]) extends Lists[T]
}