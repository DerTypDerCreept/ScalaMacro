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
	import ConvertMe._
	spts("hello")
	//ConvertMe.saySomething
	 def sum(list: Lists[Int]): Int = list.fold[Int] {
    case NilF() => 0
    case ConsF(a, l) => a + l
  }
  
	val list = Cons(1, Cons(2, Cons(3, Nil())))
    println(s"sum($list) = ${sum(list)}")
	}

	
@convert
object ConvertMe{
  trait Lists[T]

  // a case class with a name as argument is always bound
  case class Nil[T] extends Lists[T]
  case class Cons[T](head:T, tail:Lists[T]) extends Lists[T]
  //case class Whatevs[T](smthing:T)
}


