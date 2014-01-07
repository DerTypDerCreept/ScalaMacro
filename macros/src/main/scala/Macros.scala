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
				val typ = types // ++ List(q"type FFucntor") 
				q"trait $name[..$typ]"
			}
			case q"case class $name[..$types](..$fields) extends $traitname[..$types2]" => {
				println("we found a case class:"+name)
				val typ = /* List(Ident(newTypeName("FFunctor"))) ++ */  types2
				cNames = List(""+name) ++ cNames
				/* fields(0) match {
					case q"val $fname: $ftype" => {
						println("Extract the first field: "+fname+":"+ftype)
						true
					}
					case _ => false
				}*/

				
				if(""+name != "Nil"){ 
				    //println(""+fields(0)+" is:")
					println("the class"+fields(0).getClass)
					println("is type:"+fields(0).isType) //false
					println("is term:"+fields(0).isTerm) //false
					println("is def:"+fields(0).isDef) //true
					println("tpe:"+fields(0).tpe) // null -> das ding weiss selbst net was es is
					println(fields(1).asInstanceOf[ValDef].children)
					val typees: List[scala.reflect.api.Types#Type] = fields.map(_.tpe)
					println(typees)
					println("'"*75)
					println(fields)
					println("'"*75)
				}
				
				
				
				
				
				
				
				classes = List(q"case class $name[..$types](..$fields) extends $traitname[..$typ]") ++ classes
				q"case class $name[..$types](..$fields) extends $traitname[..$typ]"
			}
			case _ => x

			
		}
		def analyzeL(x:List[Tree]):List[Tree] = x match {
			case y :: z => analyze(y) :: analyzeL(z)
			case _ => x
		}

		/*
		def modify(x:Tree,y:List[Tree]):Tree = x match {
			case ModuleDef(a,b,templ) => ModuleDef(a,b,modify(templ).asInstanceOf[Template])
			case Template(a,b,list) => Template(a,b,analyzeL(list))
			case _ => x
				
			}
		}	
		def modifyL(x:List[Tree], y:List[Tree]){
			q"trait $traitname[..$types]" :: rest => {
				val typ = List(q"type FFucntor") ++ types 
				val newtrait = traitname+"F"
				//val body = q"" -- construct map
				q"trait $newtrait[..$typ]" //{..$body}"
			} ++ {
				val typ = List(q"type traitname[..$types]") ++ types
				val newtrait = traitname+"F"
				//val body = q"" -- construct fold				
				q"trait $traitname[..$types] extends $newtrait[..$typ]" //{..$body}" 
			} ++ modifyL(rest,y)
			case q"case class $name[..$types](..$fields) extends $traitname[..$types2]" =>{
				val typ = List(Ident(newTypeName("FFunctor"))) ++ types2
				val newtrait = traitname+"F"
				val newname = name+"F"
				q"case class $newname[..$typ](..$fields) extends $newtrait[..$typ]"
			} ++ {
				val typ = List(Ident(newTypeName("FFunctor"))) ++ types2
				val newname = name+"F"
				q"class $name[..$types](..$fields) extends $newname[..$types2] with $traitname[..$types2]"
			} ++ modifyL(rest,y)
		}
		*/
			
			
		val outputs = expandees
		println("?"*50)
		
		analyze(expandees(0))
		if(traitname!="FDefault"){}
		println(classes)
		println(showRaw(classes))
		//println(analyze(expandees(0)))
		//println(showRaw(expandees))
		println("?"*50)
		
		
		
		
		c.Expr[Any](Block(outputs, Literal(Constant(()))))
	}	
}


