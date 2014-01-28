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

  // test the dummy macro

  ConvertMe.saySomething

  /* test case (not functional for the moment)

  import ConvertMe._

  def sum(xs: Lists[Int]): Int = xs.fold[Int] {
    case NilF() => 0
    case ConsF(n, m) => n + m
  }
   */
}

	
@convert
object ConvertMe{
  trait Lists[T]

  // a case class with a name as argument is always bound
  case class Nil[T] extends Lists[T]
  case class Cons[T](head:T, tail:Lists[T]) extends Lists[T]

}


