import scala.reflect.macros.Context
import scala.language.experimental.macros
import scala.annotation.StaticAnnotation

class convert extends StaticAnnotation {
    def macroTransform(annottees: Any*) = macro convertMacro.impl
}

object convertMacro {
    def impl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
        import c.universe._
        
    case class FixedPoint(name: TypeName, typeParams: List[TypeDef])
    case class Variant(name: TypeName, typeParams: List[TypeDef], valParams: List[ValDef])


    case class BusinessInput(fixed: FixedPoint, variants: List[Variant])
    

  def extractDefList(x:Tree):List[Tree] = x match {
    case ModuleDef(a, b, Template(_, _, list)) => list
  }
    def findFixedPoint(raw: List[Tree]): FixedPoint = raw match {
        case q"trait $traitname[..$types]" :: tail => FixedPoint(traitname,types)
        case head :: tail => findFixedPoint(tail)
        case _ => throw new Exception("Could not find Fixed Point (no trait in annotated object)")
    }
    def findVariants(raw: List[Tree], fixed: TypeName): List[Variant] = { val fix = fixed; raw match{
        case q"case class $name[..$types](..$fields) extends $fix" :: tail
            if fix == fixed =>
          Variant(name,types,fields) :: findVariants(tail,fixed)
        case head :: tail => findVariants(tail,fixed)
        case Nil => Nil
        case _ => throw new Exception("Find Variants Malfunctioned")
    } }
	//Helper Functions
        //clean Type turns type defunitions into type references
        def cleanType(x:List[Tree]):List[Tree] = x match {
            case TypeDef(a,b,List(),d) :: rest => Ident(b) :: cleanType(rest)
            case TypeDef(a,b,c,d) :: rest => AppliedTypeTree(Ident(b),cleanType(c)) :: cleanType(rest)
            case AppliedTypeTree(a,b) :: rest => AppliedTypeTree(a,cleanType(b)) :: cleanType(rest)
            case _ => x
            }
        //listOfApply takes a list of valDefs, a type reference and a function name
        //                    and returns a List where ValDefs are replaced with a reference to them
        //                    except for when they match the given type, then the function is applied to the valDef
        def listOfApply(x:List[ValDef], typ:Tree, funName:Ident ):List[Tree] = x match {
            //case ValDef(a,b,`typ`,d) :: z => Apply(funName,List(Ident(b))) :: listOfApply(z,typ,funName) 
            case ValDef(a,b,c,d) :: z => {
                if (c.canEqual(typ)) Apply(funName,List(Ident(b))) :: listOfApply(z,typ,funName)
                else Ident(b) :: listOfApply(z,typ,funName) 
            }
            case Nil => Nil
        }
        //listOf takes a list of valDefs and returns a list of references to them
        def listOf(x:List[ValDef]):List[Ident] = x match {
            case ValDef(a,b,c,d) :: z => Ident(b) :: listOf(z) 
            case Nil => Nil
        }
        //listOfAsSelect
        def listOfAsSelect(x:List[ValDef], modification: String):List[Select] = x match {
            case ValDef(a,b,c,d) :: z => {
                val tmp=newTermName(modification); 
                q"$tmp.$b" :: listOfAsSelect(z,modification)} 
            case Nil => Nil
        }
        //updateType
        def updateType(x:List[ValDef], name:Tree, types:List[Tree], newType:String ):List[ValDef] = x match {
            case ValDef(a,b,c,d) :: z => {
                //println(showRaw(c))
                q"class ignoreMe extends $c" match {
                    case q"class ignoreMe extends $name[..$types]" => ValDef(a,b,Ident(newTypeName(newType+"")),d) :: updateType(z,name,types,newType+"") 
                    case _ => ValDef(a,b,c,d) :: updateType(z,name,types,newType+"")
                }
            }
            case Nil => Nil
            case _ => x
        }
	def expandFixedPoint(fixed: FixedPoint):List[Tree] = {
		val fixedType1 = q"type FFunctor"
		val fixedType2 = q"type FFunctor2"
		val fixedType3 = q"type FFunctor3"
		val newTraitName = newTypeName(fixed.name.toString+"F")
		val newTypeList = fixed.typeParams ++ List(fixedType1)
		val newTypeReferences2 = cleanType(fixed.typeParams ++ List(fixedType2))
		val newTypeReferences3 = cleanType(fixed.typeParams ++ List(fixedType3))
        val maps = q"def map[FFunctor2](g: FFunctor => FFunctor2): $newTraitName[..$newTypeReferences2]"
        val newBodyFixed = List(maps)
		
		
        val extendTypes = cleanType(fixed.typeParams ++ List(q"type ${fixed.name}[..${fixed.typeParams}]"))        
        val folds = q"def fold[$fixedType3](phi: $newTraitName[..$newTypeReferences3] => ${newTypeReferences3.last}): ${newTypeReferences3.last} = phi(this map (_ fold phi))"
        val newBody = List(folds)
        
        val traitFixed = List(q"trait $newTraitName[..$newTypeList]{..$newBodyFixed}")
		val traitNormal = List(q"trait ${fixed.name}[..${fixed.typeParams}] extends $newTraitName[..$extendTypes]{..$newBody}")
		
		traitFixed ++ traitNormal
		
	}

  def businessLogic(input: BusinessInput): List[Tree] = {
    expandFixedPoint(input.fixed)
  }

  def createInput(raw: List[Tree]): BusinessInput = {
    val fixed = findFixedPoint(raw)
    BusinessInput(fixed, findVariants(raw,fixed.name))
  }

  def createOutput(original: Tree, newDefs: List[Tree]): Tree =
    original match {
      case mod @ ModuleDef(a, objectName, templ) =>
        q"""
         object $objectName {
           ..${businessLogic(createInput(extractDefList(original)))}
         }"""

    }

        
        
        val inputs = annottees.map(_.tree).toList
        val (annottee, expandees) = inputs match {
            case (param: ValDef) :: (rest @ (_ :: _)) => (param, rest)
            case (param: TypeDef) :: (rest @ (_ :: _)) => (param, rest)
            case _ => (EmptyTree, inputs)
        }
        
        
        def modify(x:Tree):Tree = x match {
            case ModuleDef(a,b,templ) => ModuleDef(a,b,modify(templ).asInstanceOf[Template])
            case Template(a,b,list) => Template(a,b,modifyL(list))
            case _ => x
        }    
        def modifyL(x:List[Tree]):List[Tree] = x match {
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
                val typ2 = cleanType(types ++ List(newTypName2)) //List(q"type FFunctor2")//
                val newtrait = newTypeName(traitname+"F")
                //println("do we get here?")
                val maps = q"def map[FFunctor2](g: FFunctor => FFunctor2): $newtrait[..$typ2]"
                val newBody = List(maps)
                //val body = q"" -- construct map
                q"trait $newtrait[..$typ]{..$newBody}"
            }  :: {
                val newtrait = newTypeName(traitname+"F")

                val typ = cleanType(types ++ List(q"type $traitname[..$types]"))
                val newTypName1 = q"class Whatever[FFunctor3]" match {
                    case q"class Whatever[$myType]" => myType
                    case _ => throw new Exception("[Convert Macro]:Could not construct type by QuasiquoteMatching")
                }
                val newtyp = cleanType(types ++ List(newTypName1))//List(q"type FFunctor")//
                //val body = q"" -- construct fold    
                
                val folds = q"def fold[$newTypName1](phi: $newtrait[..$newtyp] => ${newtyp.last}): ${newtyp.last} = phi(this map (_ fold phi))"
                val newBody = List(folds)
                q"trait $traitname[..$types] extends $newtrait[..$typ]{..$newBody}" 
            } :: modifyL(rest)
            case q"case class $name[..$types](..$fields) extends $traitname[..$types2]" :: rest =>{
                val newtrait = newTypeName(traitname+"F")
                val oldtrait = newTypeName(traitname+"")
                val typeRef = cleanType(List(q"type $oldtrait[..$types]"))
                val newName = newTypeName(name+"F")
                val newNameTerm = Ident(newTermName(name+"F"))
                val newTypName1 = q"class Whatever[FFunctor]" match {
                    case q"class Whatever[$myType]" => myType
                    case _ => throw new Exception("[Convert Macro]:Could not construct type by QuasiquoteMatching")
                }
                val newTypName2 = q"class Whatever[FFunctor2]" match {
                    case q"class Whatever[$myType]" => myType
                    case _ => throw new Exception("[Convert Macro]:Could not construct type by QuasiquoteMatching")
                }
                val typ = types ++List(newTypName1)  //List(q"type FFunctor")//
                val typ2 = cleanType(types ++ List(newTypName2)) //List(q"type FFunctor2") //
                val fieldnames = listOf(fields)
                val fieldnames2 = listOfApply(fields,typeRef.head,Ident(newTermName("g")))
                val newFields = updateType(fields,traitname,types,"FFunctor")
                
                var maps= q"def map[FFunctor2](g: FFunctor => FFunctor2): $newtrait[..$typ2] = $newNameTerm[..$typ2]()" //(..$fieldnames)"
                if(fieldnames.length!=0)
                    maps = q"def map[FFunctor2](g: FFunctor => FFunctor2): $newtrait[..$typ2] = ${Ident(newName)}(..$fieldnames2)"
                val newBody = List(maps)

                q"case class $newName[..$typ](..$newFields) extends $newtrait[..${cleanType(typ)}]{..$newBody}"
            } ::  {
                //val typ = List(Ident(newTypeName("FFunctor"))) ++ types2
                //val newtrait = newTypeName(traitname+"F")
                //val oldtrait = newTypeName(traitname+"")
                
                val fieldnames = listOf(fields)
                val newNameType = Ident(newTypeName(name+"F"))
                //val newNameType = Ident(newTermName(name+"F"))
                //val app = q"$newNameTerm(..$fieldnames)"
                val tt = q"$newNameType(..$fieldnames)"
                println("::"+tt)
                val xxx=q"class Cons[T](head: T, tail: List[T]) extends ConsFs(heads, tails) with List[T]" match{
                    case q"class Cons[T](head: T, tail: List[T]) extends $xx(..$xxs) with List[T]" =>{
                    //case q"class Cons[T](head: T, tail: List[T]) extends $xx with List[T]" =>{
                    println(";"*75)
                    println(showRaw(xx))
                    //println(showRaw(xxs))
                    println(showRaw(fieldnames))
                    println(showRaw(newNameType))
                    }
                    case _ => println("hm")
                }
                
                val newname = newTypeName(name+"F")
                val typ2 = q"class Whatever extends $traitname[..$types]" match {
                    case q"class Whatever extends $type2" => List(type2)
                    case _ => throw new Exception("[Convert Macro]:Could not construct type by QuasiquoteMatching")
                }
                val typ = cleanType(types ++ typ2)
                //causes stack overflow
                if(fieldnames.length<1)    q"class $name[..$types](..$fields) extends $newname[..$typ] with $traitname[..$types2]"
                else q"class $name[..$types](..$fields) extends $tt with $traitname[..$types2]"
            
            } :: 
            {
                val nam = newTermName(name+"")
                val fieldnames = listOf(fields)
                val fieldSelects = listOfAsSelect(fields,"u")
                val newNameTerm = Ident(newTermName(name+""))
                val newTypes = cleanType(types)
                val app = q"def apply[..$types](..$fields):$traitname[..$newTypes] = new $newNameTerm(..$fieldnames)"
                val unapp = q"def unapply[..$types](u: $name[..$newTypes]):Option[Unit] = Some((..$fieldSelects))"
                val newBody = List(app) ++ List(unapp)
                //println(theList)
                q"object $nam{..$newBody}"
            } ::  modifyL(rest) 
            case a::b => a::modifyL(b)
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
        //val res = modify(expandees(0))
        //println(res)
        
        //if(traitname!="FDefault"){}
        //println(classes)
        //println(showRaw(classes))
        //println(analyze(expandees(0)))
        //println(showRaw(expandees))
        println("?"*50)

      println(businessLogic(createInput(extractDefList(expandees(0)))))

      val output: Tree =
        createOutput(
          expandees.head,
          businessLogic(createInput(extractDefList(outputs.head))))

      println(output)
      c.Expr[Any](Block(List(output), Literal(Constant(()))))
    }    
}


