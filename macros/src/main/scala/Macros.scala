import scala.reflect.macros.Context
import scala.language.experimental.macros
import scala.annotation.StaticAnnotation


object DefMacros {
  //hello world macro
  def hello(): Unit = macro hello_impl
  def hello_impl(c: Context)(): c.Expr[Unit] = {
	import c.universe._
	reify { println("Hello World!") }
  }
  
  //hello world macro, constructing AST for "Hello World!" and then splicing it in 
  def hello1(): Unit = macro hello1_impl
  def hello1_impl(c: Context)(): c.Expr[Unit] = {
	import c.universe._
	//construct the AST for "Hello World!" String
	var x = c.Expr(Literal(Constant("Hello World!")))
	reify { println(x.splice) }
  }
  
  //currently just a println for anything with toString
  def hello2[T](param : T): Unit = macro hello2_impl
  def hello2_impl(c: Context)(param: c.Expr[Any]): c.Expr[Unit] = {
	import c.universe._
	//just splice the parameter in (this gives the value of the parameter)
	reify { println(param.splice) }
  }
  //experimenting with printing trees
  //aww yeah, showRaw gives a String representation of the tree
  def hello2_1[T](param : T): Unit = macro hello2_1_impl
  def hello2_1_impl(c: Context)(param: c.Expr[Any]): c.Expr[Unit] = {
	import c.universe._
	//use showRaw on param.tree to get a String representation of the tree
	//then we have to build an AST for the String, to be able to splice it in
	val x = c.Expr(Literal(Constant(showRaw(param.tree))))
	reify { println(x.splice) }
  }
  
  //from the example: print a parameter's name and value
  def debug(param: Any): Unit = macro debug_impl
  def debug_impl(c: Context)(param: c.Expr[Any]): c.Expr[Unit] = {
    import c.universe._
    //first extract the value
    val paramRep = show(param.tree)
    //build an AST
    val paramRepTree = Literal(Constant(paramRep))
    val paramRepExpr = c.Expr[String](paramRepTree)
    //this part I don't get, this makes no sense
    reify { println(paramRepExpr.splice + " = " + param.splice) }
  }
  
  //function selector
  def funSelect(param: String):Int => Int = macro funSelectImpl
  def funSelectImpl(c: Context)(param: c.Expr[String]):c.Expr[Int => Int] = {
    import c.universe._
    var (Literal(Constant(x)))=param.tree
    //throw new Exception(""+show(param.tree))
    if(x == "+") reify((y) => y + y)
    else if(x  == "*") reify((y) => y * y)
    else  reify((y) => y)
  }
  

  //macros to print the methods, vars and vals of a class	 
	def methodNames[A]: List[String] = macro methodNames_impl[A]
	  def methodNames_impl[A : c.WeakTypeTag](c: Context): c.Expr[List[String]] = {
	    import c.universe._
	 
	    val methods: List[String] = c.weakTypeOf[A].typeSymbol.typeSignature.
	      declarations.toList.filter((z) => z.isMethod).map(_.name.toString)
	    val listApply = Select(reify(List).tree, newTermName("apply"))
	 
	    c.Expr[List[String]](Apply(listApply,
	      List(methods.map(x => Literal(Constant(x))):_*)))
	  }
	  
	  def varNames[A]: List[String] = macro varNames_impl[A]
	  def varNames_impl[A : c.WeakTypeTag](c: Context): c.Expr[List[String]] = {
	    import c.universe._
	 
	    val methods: List[String] = c.weakTypeOf[A].typeSymbol.typeSignature.
	      declarations.toList.filter((z) => z.isTerm && z.asTerm.isVar).map(_.name.toString)
	    val listApply = Select(reify(List).tree, newTermName("apply"))
	 
	    c.Expr[List[String]](Apply(listApply,
	      List(methods.map(x => Literal(Constant(x))):_*)))
	  }
	  
	  def valNames[A]: List[String] = macro valNames_impl[A]
	  def valNames_impl[A : c.WeakTypeTag](c: Context): c.Expr[List[String]] = {
	    import c.universe._
	 
	    val methods: List[String] = c.weakTypeOf[A].typeSymbol.typeSignature.
	      declarations.toList.filter((z) => z.isTerm && z.asTerm.isVal).map(_.name.toString)
	    val listApply = Select(reify(List).tree, newTermName("apply"))
	 
	    c.Expr[List[String]](Apply(listApply,
	      List(methods.map(x => Literal(Constant(x))):_*)))
	 
		}
	
  
}

class identity extends StaticAnnotation {
	def macroTransform(annottees: Any*) = macro identityMacro.impl
}

object identityMacro {
	def impl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
		import c.universe._
		val inputs = annottees.map(_.tree).toList
		val (annottee, expandees) = inputs match {
		    case (param: ValDef) :: (rest @ (_ :: _)) => (param, rest)
			case (param: TypeDef) :: (rest @ (_ :: _)) => (param, rest)
			case _ => (EmptyTree, inputs)
		}
		//println((annottee, expandees))
		val outputs = expandees
		c.Expr[Any](Block(outputs, Literal(Constant(()))))
	}
}

class appendLol extends StaticAnnotation {
	def macroTransform(annottees: Any*) = macro appendLolMacro.impl
}

object appendLolMacro {
	def impl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
		import c.universe._
		println("="*75)
		println(("="*29)+"Append Lol Makro"+("="*30))
		println("="*75)
		val inputs = annottees.map(_.tree).toList
		val (annottee, expandees) = inputs match {
		    case (param: ValDef) :: (rest @ (_ :: _)) => throw new Exception("Not Intended for this Use")
			case (param: TypeDef) :: (rest @ (_ :: _)) => throw new Exception("Not Intended for this Use")
			case _ => (EmptyTree, inputs)
		}
		def traverse(x:Tree):Tree = x match {
			case ClassDef(a,b,c,d) => ClassDef(a,b,c,traverse(d).asInstanceOf[Template])
			case Template(a,b,c) => Template(a,b,traverseL(c))
			case ValDef(a,b,c,d) => ValDef(a,b+"lol",c,d)
			case DefDef(a,b,c,List(d),e,f) => DefDef(a,b,c,List(traverseL(d).asInstanceOf[List[ValDef]]),e,traverse(f).asInstanceOf[Apply])
			case DefDef(a,b,c,d,e,f) => DefDef(a,b,c,d,e,traverse(f).asInstanceOf[Apply])
			case Apply(Select(a,b),c) => Apply(Select(traverse(a),b),traverseL(c))
			case Apply(a,b) => Apply(traverse(a),traverseL(b))
			case Select(a,b) => Select(a,traverseL(b.asInstanceOf[List[Tree]])(0).asInstanceOf[Symbol])
			case Ident(a) => Ident(a+"lol")
			case _ => x
		}
		
		def traverseL(x:List[Tree]):List[Tree] = x match {
			case y :: z => traverse(y) :: traverseL(z)
			case _ => x
		}
		val newC = traverse(expandees(0))
		println("During Compilation - this is Class C:")
		println(expandees(0))
		println("="*75)
		println("During Compilation - this is the modified Class C:")
		println(newC)
		val outputs = List(newC)
		c.Expr[Any](Block(outputs, Literal(Constant(()))))
	}
}

class addMember extends StaticAnnotation {
	def macroTransform(annottees: Any*) = macro addMemberMacro.impl
}

object addMemberMacro {
	def impl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
		import c.universe._
		println("="*75)
		println(("="*29)+"Add Member makro"+("="*30))
		println("="*75)
		val inputs = annottees.map(_.tree).toList
		val (annottee, expandees) = inputs match {
		    case (param: ValDef) :: (rest @ (_ :: _)) => throw new Exception("Not Intended for this Use")
			case (param: TypeDef) :: (rest @ (_ :: _)) => throw new Exception("Not Intended for this Use")
			case _ => (EmptyTree, inputs)
		}
		def traverse(x:Tree):Tree = x match {
			case ClassDef(a,b,c,d) => ClassDef(a,b,c,traverse(d).asInstanceOf[Template])
			case Template(a,b,c) => Template(a,b,traverseL(c))
			case _ => x
		}
		
		def traverseL(x:List[Tree]):List[Tree] = x match {
			case y :: List() => {y 	:: DefDef(Modifiers(), newTermName("foo"), List(), List(), TypeTree(), Literal(Constant("Hello World"))) ::	DefDef(Modifiers(), newTermName("bar"), List(), List(), TypeTree(), Literal(Constant(42))) :: List()}
			case y :: z => y :: traverseL(z)
			case _ => x
		}
		
		println(traverse(expandees(0)))
		
		c.Expr[Any](Block(List(traverse(expandees(0))), Literal(Constant(()))))
	}
}
class addMemberQuasi extends StaticAnnotation {
	def macroTransform(annottees: Any*) = macro addMemberQuasiMacro.impl
}

object addMemberQuasiMacro {
	def impl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
		import c.universe._
		println("="*75)
		println(("="*26)+"Add Quasi Member makro"+("="*27))
		println("="*75)
		val inputs = annottees.map(_.tree).toList
		val (annottee, expandees) = inputs match {
		    case (param: ValDef) :: (rest @ (_ :: _)) => throw new Exception("Not Intended for this Use")
			case (param: TypeDef) :: (rest @ (_ :: _)) => throw new Exception("Not Intended for this Use")
			case _ => (EmptyTree, inputs)
		}
		def traverse(x:Tree):Tree = x match {
			case ClassDef(a,b,c,d) => ClassDef(a,b,c,traverse(d).asInstanceOf[Template])
			case Template(a,b,c) => Template(a,b,traverseL(c))
			case _ => x
		}
		
		def traverseL(x:List[Tree]):List[Tree] = x match {
			case y :: List() => {y 	:: q"""def quasifoo = "Hello World" """ :: q"def quasibar = 42" :: List()}
			case y :: z => y :: traverseL(z)
			case _ => x
		}
		
		println(traverse(expandees(0)))
		
		c.Expr[Any](Block(List(traverse(expandees(0))), Literal(Constant(()))))
	}
}

//Example Hello World macros by Eugene Burmako
//from the End To End Setup Examples
object ExampleHello {
  def impl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._
    import Flag._
    val result = {
      annottees.map(_.tree).toList match {
        case ModuleDef(mods, name, Template(parents, self, body)) :: Nil =>
          val helloMethod = DefDef(NoMods, newTermName("hello"), List(), List(List()), TypeTree(), Literal(Constant("hello")))
          ModuleDef(mods, name, Template(parents, self, body :+ helloMethod))
      }
    }
    c.Expr[Any](result)
  }
}
class ExampleHelloC extends StaticAnnotation {
  def macroTransform(annottees: Any*) = macro ExampleHello.impl
}
object ExampleHelloWorld {
  def impl(c: Context) = c.universe.reify(println("hello world!"))
  def hello = macro impl
}
