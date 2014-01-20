import scala.reflect.macros.Context
import scala.language.experimental.macros
import scala.annotation.StaticAnnotation

class convert extends StaticAnnotation {
	def macroTransform(annottees: Any*) = macro convertMacro.impl
}

object convertMacro {
	def impl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
		import c.universe._
		val inputs = annottees.map(_.tree).toList
		val (annottee, expandees) = inputs match {
		    case (param: ValDef) :: (rest @ (_ :: _)) => (param, rest)
			case (param: TypeDef) :: (rest @ (_ :: _)) => (param, rest)
			case _ => (EmptyTree, inputs)
		}
		var traitname = "FDefault"
		var classes : List[Tree] = List()
		var cNames: List[String] = List()
		def analyze(x:Tree):Tree = x match {
			case ModuleDef(a,b,templ) => ModuleDef(a,b,analyze(templ).asInstanceOf[Template])
			case Template(a,b,list) => Template(a,b,analyzeL(list))
			case q"trait $name[..$types]" => {
				traitname = ""+name
				println("we found a trait:"+name)
				
				println(types(0))
				println(showRaw(types))
				val typ = types
				q"trait $name[..$typ]"
			}
			case q"case class $name[..$types](..$fields) extends $traitname[..$types2]" => {
				println("we found a case class:"+name)
				val typ = types2
				cNames = List(""+name) ++ cNames				
				classes = List(q"case class $name[..$types](..$fields) extends $traitname[..$typ]") ++ classes
				q"case class $name[..$types](..$fields) extends $traitname[..$typ]"
			}
			case _ => x
		}
		def analyzeL(x:List[Tree]):List[Tree] = x match {
			case y :: z => analyze(y) :: analyzeL(z)
			case _ => x
		}
		
		def listOf(x:List[Tree]):List[TermName] = x match {
			//case ValDef(a,b,c,d) -> not possible causes stack overflow
			case a :: rest => {
				val itera = a.asInstanceOf[ValDef].productIterator
				var i=0
				var result = newTermName("DEFAULT")
				while(itera.hasNext){
					val it = itera.next
					if(i==1){ result = it.asInstanceOf[TermName]}
					i = i+1 
					}
				result	
			} :: listOf(rest)
			case Nil => Nil
		}
		
		def modify(x:Tree,y:List[Tree]):Tree = x match {
			case ModuleDef(a,b,templ) => ModuleDef(a,b,modify(templ,y).asInstanceOf[Template])
			case Template(a,b,list) => Template(a,b,modifyL(list,list))
			case _ => x
		}	
		def modifyL(x:List[Tree], y:List[Tree]):List[Tree] = x match {
			case q"trait $traitname[..$types]" :: rest => {
				val typ = List(q"type FFucntor") ++ types 
				val newtrait = newTypeName(traitname+"F")
				println("do we get here?")
				
				//val body = q"" -- construct map
				q"trait $newtrait[..$typ]" //{..$body}"
			}  :: {
				val newtrait = newTypeName(traitname+"F")
				val typ = List(q"type $traitname[..$types]") ++ types
				//val body = q"" -- construct fold				
				q"trait $traitname[..$types] extends $newtrait[..$typ]" //{..$body}" 
			} :: modifyL(rest,y)
			case q"case class $name[..$types](..$fields) extends $traitname[..$types2]" :: rest =>{
				//val typ = List(Ident(newTypeName("FFunctor"))) ++ types2

				//val typ = List(q"type FFucntor") ++ types 
				//val typ2 = List(q"type FFucntor") ++ types2 
				val newtrait = newTypeName(traitname+"F")
				val newName = newTypeName(name+"F")
				val theList = listOf(fields)
				println(theList)
				
				//val bodyA = q"def apply[..$types](..$fields):$name[..$types] = new $name($theList)"
				//val nam = newTermName("hello")
				//println(q"object Obj")
				//modifiy fields
				

				q"case class $newName[..$types](..$fields) extends $newtrait[..$types2]"
			} ::  modifyL(rest,y) /* {
				//val typ = List(Ident(newTypeName("FFunctor"))) ++ types2
				val newtrait = newTypeName(traitname+"F")
				val newname = newTypeName(name+"F")
				//causes stack overflow
				q"class $name[..$types](..$fields) extends $newname[..$types2] with $traitname[..$types2]"
			} :: 
			{
			    //causes stack overflow
				//val bodyA = q"def apply[..$types](..$fields):$name[..$types] = new $name(..fields)"
				//val bodyU = q"def unapply[..$types](..$fields):Option[] = Some()"
				val nam = newTermName(name+"")
				q"object $nam"
			} ::  modifyL(rest,y) 
			*/
			case a::b => a::modifyL(b,y)
			case _ => x
		}

		val outputs = expandees
		println("?"*50)
		//println(q"new Cons(head, tail)")
		//println(showRaw(q"new Cons(head, tail)"))
		println("analyze")
		//analyze(expandees(0))
		println("modify")
		println(modify(expandees(0),expandees))
		if(traitname!="FDefault"){}
		//println(classes)
		//println(showRaw(classes))
		//println(analyze(expandees(0)))
		//println(showRaw(expandees))
		println("?"*50)
		
		
		
		
		
		c.Expr[Any](Block(outputs, Literal(Constant(()))))
	}	
}


