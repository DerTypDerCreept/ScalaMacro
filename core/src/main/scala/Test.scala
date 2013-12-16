object Helper{
	//print test seperator
	def pts (text: String, seperator: String, length: Int){
		println(seperator * length)
		println(text)
	}
	//standard test seperator
	def spts(text: String) = pts(text,"=",75)
}

@ExampleHelloC
object Test extends App {
    import Helper.spts
	println("Example Hello World Macros by Eugene Burmako")
	ExampleHelloWorld.hello
	println(this.hello)
	val x = 5
	spts("Hello world Makro:")
	DefMacros.hello()
	spts("Hello World! Makro with AST Construction:")
	DefMacros.hello1()
	spts("simple print macro on an Instance of class A:")
	DefMacros.hello2(new A(1,2,(v)=>v+2))
	spts("Debug Macro (on val x = 5):")
	DefMacros.debug(x)
	spts("funSelect macro + and * on x:")
	println("5+5="+(DefMacros.funSelect("+")(x)))
	println("5*5="+DefMacros.funSelect("*")(x))
	spts("methodNames and valNames Macro on class A:")
	println("Defs of A: "+DefMacros.methodNames[A])
	println("Vals of A: "+DefMacros.valNames[A])
	spts("print the AST of an instance of class A:")
	DefMacros.hello2_1(new A(1,2,(v)=>v+2))
	spts("rename vals makro (should return 1) (view change during compilation)")
	val c = new C(1,2,(v)=>v+2)
	//println(c)
	println(c.getX)
	spts("add foo and bar makro (should not compile if it does not work)")
	val d = new D(1,2,(v)=>v+2)
	println(d.foo)
	println(d.bar)
	spts("and now the same macro using quasi quotes")
	val e = new E(1,2,(v)=>v+2)
	println(e.quasifoo)
	println(e.quasibar)
	}
	
class A(x: Int, y: Int, f:(Int)=>Int){
	def foo = "Hello World"
	def bar = 42
	override def toString():String = "A(x:"+x+",y:"+y+")"
}
//identity macro-annotation
//returns the anotee, but prints it during compile time 
@identity
class B(x: Int, y: Int, f:(Int)=>Int){
	def foo = "Hello World"
	def bar = 42
	override def toString():String = "B(x:"+x+",y:"+y+")"
}
@appendLol
class C(x: Int, y: Int, f:(Int)=>Int){
	def foo = "Hello World"
	def bar = 42
	override def toString():String = "C(x:"+x+",y:"+y+")"
	def getX = x
}
//foo and bar are missing
//lets add them via a makro
@addMember
class D(x: Int, y: Int, f:(Int)=>Int){
	override def toString():String = "D(x:"+x+",y:"+y+")"
	def getX = x
}
//foo and bar are missing
//lets add them via a makro
@addMemberQuasi
class E(x: Int, y: Int, f:(Int)=>Int){
	override def toString():String = "E(x:"+x+",y:"+y+")"
	def getX = x
}


