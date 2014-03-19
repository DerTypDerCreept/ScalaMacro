object Helper{
    //print test seperator
    def pts (text: String, seperator: String, length: Int){
        println(seperator * length)
        println(text)
    }
    //standard test seperator
    def spts(text: String) = pts(text,"=",75)
}





object Test extends App with ConvertMe with Expr{
    import Helper.spts
    //import ConvertMe._
    spts("hello")
    //ConvertMe.saySomething
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
	  val value = Mul(Add(Num(5),Num(5)),Num(2))
	  val n = Num(5)
	  val m = Num.unapply(n)
	  println(m)
	  println(value)
	  
	  val p = n match {
		case Num(n) => n+1
	  }
	  println(p)
	  
	  def eval(e:Exp):Int = e match{
		case Num(a) => a
		case Add(a,b) => eval(a)+eval(b)
		case Mul(a,b) => eval(a)*eval(b)
		
	  }
	  
	  println(eval(value))
	  
	  def foldExp(e: Exp) : Int = e.fold[Int]{
		case Num(n) => n
		//case Id(x) => x
		case Add(lhs,rhs) => lhs + rhs
		case Mul(lhs,rhs) => lhs * rhs
	  }
	  
	  println(s"exp1 = ${foldExp(value)}")
	
	}

    	
@convert
trait ConvertMe{
  trait Lists[T]

  // a case class with a name as argument is always bound
  case class Nil[T] extends Lists[T]
  case class Cons[T](head:T, tail:Lists[T]) extends Lists[T]
}
/* /@convert
trait Expr{
	trait Exp[T]
	case class Num[T](n: T) extends Exp[T]
	case class Add[T](lhs: Exp[T], rhs: Exp[T]) extends Exp[T]
	case class Mul[T](lhs: Exp[T], rhs: Exp[T]) extends Exp[T]
	//case class Id(x: Symbol) extends Exp 
}*/
@convert
trait Expr{
	trait Exp
	case class Num(n: Int) extends Exp
	case class Add(lhs: Exp, rhs: Exp) extends Exp
	case class Mul(lhs: Exp, rhs: Exp) extends Exp
	case class Id(x: Symbol) extends Exp 
}
