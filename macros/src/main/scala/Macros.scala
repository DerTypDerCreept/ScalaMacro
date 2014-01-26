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
				//println("we found a trait:"+name)
				//println(types(0))
				//println(showRaw(types))
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
		
		def listOf(x:List[ValDef]):List[Ident] = x match {
			case ValDef(a,b,c,d) :: z => Ident(b) :: listOf(z) 
			case Nil => Nil
		}
		def listOfAsSelect(x:List[ValDef], modification: String):List[Select] = x match {
			case ValDef(a,b,c,d) :: z => {
				val tmp=newTermName(modification); 
				q"$tmp.$b" :: listOfAsSelect(z,modification)} 
			case Nil => Nil
		}
		def updateType(x:List[ValDef], name:Tree, types:List[Tree], newType:String ):List[ValDef] = x match {
			case ValDef(a,b,c,d) :: z => {
				println(showRaw(c))
				q"class ignoreMe extends $c" match {
					case q"class ignoreMe extends $name[..$types]" => ValDef(a,b,Ident(newTypeName(newType+"")),d) :: updateType(z,name,types,newType+"") 
					case _ => ValDef(a,b,c,d) :: updateType(z,name,types,newType+"")
				}
				/*
				if(c == oldType) 
					{
					println("incoming type:"+showRaw(c)+" to "+Ident(newTypeName(newType+"")))
					ValDef(a,b,Ident(newTypeName(newType+"")),d) :: updateType(z,oldType,newType+"") 
					}
				else
					ValDef(a,b,c,d) :: updateType(z,oldType,newType+"")
					*/
			}
			case Nil => Nil
			case _ => x
		}
		
		def modify(x:Tree,y:List[Tree]):Tree = x match {
			case ModuleDef(a,b,templ) => ModuleDef(a,b,modify(templ,y).asInstanceOf[Template])
			case Template(a,b,list) => Template(a,b,modifyL(list,list))
			case _ => x
		}	
		def modifyL(x:List[Tree], y:List[Tree]):List[Tree] = x match {
			case q"trait $traitname[..$types]" :: rest => {
				val newTypName1 = q"class Whatever[FFunctor]" match {
					case q"class Whatever[$myType]" => myType
					case _ => throw new Exception("[Convert Macro]:Could not construct type by QuasiquoteMatching")
				}
				val newTypName2 = q"class Whatever[FFunctor2]" match {
					case q"class Whatever[$myType]" => myType
					case _ => throw new Exception("[Convert Macro]:Could not construct type by QuasiquoteMatching")
				}
				val typ = types ++ List(newTypName1) //List(q"type FFunctor")// 
				val typ2 = types ++ List(newTypName2) //List(q"type FFunctor2")//
				val newtrait = newTypeName(traitname+"F")
				//println("do we get here?")
				val maps = q"def map[FFunctor2](g: FFunctor => FFunctor2): $newtrait[..$typ2]"
				val newBody = List(maps)
				//val body = q"" -- construct map
				q"trait $newtrait[..$typ]{..$newBody}"
			}  :: {
				val newtrait = newTypeName(traitname+"F")

				val typ = types ++ List(q"type $traitname[..$types]")
				val newTypName1 = q"class Whatever[FFunctor]" match {
					case q"class Whatever[$myType]" => myType
					case _ => throw new Exception("[Convert Macro]:Could not construct type by QuasiquoteMatching")
				}
				val newtyp = types ++ List(newTypName1)//List(q"type FFunctor")//
				//val body = q"" -- construct fold	
				
				val folds = q"def fold[${newtyp.last}](phi: $newtrait[..$newtyp] => ${newtyp.last}): ${newtyp.last} = phi(this map (_ fold phi))"
				val newBody = List(folds)
				q"trait $traitname[..$types] extends $newtrait[..$typ]{..$newBody}" 
			} :: modifyL(rest,y)
			case q"case class $name[..$types](..$fields) extends $traitname[..$types2]" :: rest =>{
				//val typ = List(Ident(newTypeName("FFunctor"))) ++ types2

				//val typ = List(q"type FFunctor") ++ types 
				//val typ2 = List(q"type FFunctor") ++ types2 
				val newtrait = newTypeName(traitname+"F")
				val newName = newTypeName(name+"F")
				val newTypName1 = q"class Whatever[FFunctor]" match {
					case q"class Whatever[$myType]" => myType
					case _ => throw new Exception("[Convert Macro]:Could not construct type by QuasiquoteMatching")
				}
				val newTypName2 = q"class Whatever[FFunctor2]" match {
					case q"class Whatever[$myType]" => myType
					case _ => throw new Exception("[Convert Macro]:Could not construct type by QuasiquoteMatching")
				}
				val typ = types ++List(newTypName1)  //List(q"type FFunctor")//
				val typ2 = types ++ List(newTypName2) //List(q"type FFunctor2") //
				val fieldnames = listOf(fields)
				val newFields = updateType(fields,traitname,types,"FFunctor")
				  //case NilF() => NilF[T, M]()
      //case ConsF(head, tail) => ConsF(head, g(tail))				
				var maps= q"def map[M](g: FFunctor => M): ListsF[T, M] = NilF[T, M]()" //(..$fieldnames)"
				if(fieldnames.length!=0)
					maps = q"def map[M](g: FFunctor => M): ListsF[T, M] =  ConsF(head, g(tail))"
				
				//var maps= q"def map[FFunctor2](g: FFunctor => FFunctor2): $newtrait[..$typ2] = $newName[..$typ2]()" //(..$fieldnames)"
				//if(fieldnames.length!=0)
				//	maps = q"def map[FFunctor2](g: FFunctor => FFunctor2): $newtrait[..$typ2] = $newName(..$fieldnames)"
				val newBody = List(maps)

				q"case class $newName[..$typ](..$newFields) extends $newtrait[..$typ]{..$newBody}"
			} ::  {
				//val typ = List(Ident(newTypeName("FFunctor"))) ++ types2
				val newtrait = newTypeName(traitname+"F")
				val oldtrait = newTypeName(traitname+"") //not shure why I need to reconstruct this here
				val newname = newTypeName(name+"F")
				val typ = types ++ List(q"type $oldtrait[..$types]")
				//causes stack overflow
				q"class $name[..$types](..$fields) extends $newname[..$typ] with $traitname[..$types2]"
			} :: 
			{
			    //causes stack overflow
				//val bodyA = q"def apply[..$types](..$fields):$name[..$types] = new $name(..fields)"
				//val bodyU = q"def unapply[..$types](..$fields):Option[] = Some()"
				val nam = newTermName(name+"")
				val fieldnames = listOf(fields)
				val fieldSelects = listOfAsSelect(fields,"u")
				val app = q"def apply[..$types](..$fields):$traitname[..$types] = new $name(..$fieldnames)"
				val unapp = q"def unapply[..$types](u: $name[..$types]):Option[Unit] = Some((..$fieldSelects))"
				val newBody = List(app) ++ List(unapp)
				//println(theList)
				q"object $nam{..$newBody}"
			} ::  modifyL(rest,y) 
			case a::b => a::modifyL(b,y)
			case _ => x
		}

		val outputs = expandees
		println("?"*50)
		//println(q"new Cons(head, tail)")
		//println(showRaw(q"new Cons(head, tail)"))
		//println("analyze")
		//analyze(expandees(0))
		println("The Original")
		println(outputs)
		println("?"*50)
		println("The Modified")
		
		println(modify(expandees(0),expandees))
		
		if(traitname!="FDefault"){}
		//println(classes)
		//println(showRaw(classes))
		//println(analyze(expandees(0)))
		//println(showRaw(expandees))
		println("?"*50)
		
		
		
		
		
		//c.Expr[Any](Block(modify(expandees(0),expandees), Literal(Constant(()))))
		c.Expr[Any](Block(outputs, Literal(Constant(()))))
	}	
}


