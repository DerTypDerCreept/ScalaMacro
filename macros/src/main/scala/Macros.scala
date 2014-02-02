import scala.reflect.macros.Context
import scala.language.experimental.macros
import scala.annotation.StaticAnnotation

class convert extends StaticAnnotation {
    def macroTransform(annottees: Any*) = macro convertMacro.impl
}

object convertMacro {
    def impl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
        import c.universe._
        
	//The new interface
	//represents the trait, we wish to treat as the fixed Point
    case class FixedPoint(name: TypeName, typeParams: List[TypeDef])
	//represents a case class
    case class Variant(name: TypeName, typeParams: List[TypeDef], valParams: List[ValDef])
	//contains a FixedPoint and a list of variants
    case class BusinessInput(fixed: FixedPoint, variants: List[Variant])
    
	//extracts the body of a ModuleDef
  def extractDefList(x:Tree):List[Tree] = x match {
    case ModuleDef(a, b, Template(_, _, list)) => list
	case _ => throw new Exception("Could not extract Def List from"+x)
  }
	//takes a list of Trees and returns the first trait
	//as an instance of FixedPoint
    def findFixedPoint(raw: List[Tree]): FixedPoint = raw match {
        case q"trait $traitname[..$types]" :: tail => FixedPoint(traitname,reconstructTypes(types).asInstanceOf[List[TypeDef]])
        case head :: tail => findFixedPoint(tail)
        case _ => throw new Exception("Could not find Fixed Point (no trait in annotated object)")
    }
	//takes a list of Trees  and the name of a Type
	//and filters the list for case classes extending the given type
	//turning them into Variants
    def findVariants(raw: List[Tree], fixed: TypeName): List[Variant] = raw match{
        case q"case class $name[..$types](..$fields) extends $fix[..$smth]" :: tail
          if fix.toString == fixed.toString => 
			Variant(name,reconstructTypes(types).asInstanceOf[List[TypeDef]],fields) :: findVariants(tail,fixed)
        case head :: tail => findVariants(tail,fixed)
        case Nil => Nil
        case _ => throw new Exception("Find Variants Malfunctioned")
    }
	
	def reconstructTypes(x:List[Tree]):List[Tree] = x match{
		case TypeDef(a,b,c,d) :: tail => TypeDef(a,b,c,reconstructTypesSub(d)) :: reconstructTypes(tail)
		case AppliedTypeTree(a,b) :: tail => AppliedTypeTree(reconstructTypes(List(a)).head,reconstructTypes(b)) :: reconstructTypes(tail)
		//case AppliedTypeTree(Select(Select(a, b), c), d) :: tail => AppliedTypeTree(Select(Select(a, newTermName(b.toString)), c), d) :: reconstructTypes(tail) 
		//case Select(Select(a, b), c) :: tail => Select(Select(a, newTermName(b.toString)), c) :: reconstructTypes(tail) 
		case head :: tail => head :: reconstructTypes(tail)
		case Nil => Nil
	}
	def reconstructTypesSub(x:Tree):Tree = x match{
		case TypeBoundsTree(Select(Select(a, b), c), Select(Select(d,e), f)) => TypeBoundsTree(Select(Select(a, newTermName(b.toString)), c), Select(Select(d,newTermName(e.toString)), f))
        case _ => x		
	}
	
	//Helper Functions
        //clean Type takes a list of TypeDefs or AppliedTrees and
		//returns a list of references to these types
        def typeDefsToTypeRefs(x:List[Tree]):List[Tree] = x match {
            case TypeDef(a,b,List(),d) :: rest => Ident(b) :: typeDefsToTypeRefs(rest)
            case TypeDef(a,b,c,d) :: rest => AppliedTypeTree(Ident(b),typeDefsToTypeRefs(c)) :: typeDefsToTypeRefs(rest)
            case AppliedTypeTree(a,b) :: rest => AppliedTypeTree(a,typeDefsToTypeRefs(b)) :: typeDefsToTypeRefs(rest)
            case _ => x
            }
        //applyDefinedValsOfTypeTo takes a list of valDefs, a type reference and a function name
        //                    and returns a List, where ValDefs are replaced with a reference to them
        //                    except for when the parameters they define are of the given type, then the
		//				      function is applied to the parameter defined by the current ValDef
        def applyDefinedValsOfTypeTo(x:List[ValDef], typ:Tree, funName:Ident ):List[Tree] = x match {
            //case ValDef(a,b,`typ`,d) :: z => Apply(funName,List(Ident(b))) :: applyDefinedValsOfTypeTo(z,typ,funName) 
            case ValDef(a,b,c,d) :: z => {
                if (c.canEqual(typ)) Apply(funName,List(Ident(b))) :: applyDefinedValsOfTypeTo(z,typ,funName)
                else Ident(b) :: applyDefinedValsOfTypeTo(z,typ,funName) 
            }
            case Nil => Nil
        }
        //valDefsToValRefs takes a list of valDefs and returns a list of references to the parameters
		//defined by them (wrapping their names in Idents)
        def valDefsToValRefs(x:List[ValDef]):List[Ident] = x match {
            case ValDef(a,b,c,d) :: z => Ident(b) :: valDefsToValRefs(z) 
            case Nil => Nil
        }
        //valDefsToSelect
		//takes a list of ValDefs and a String and returns a list of selects, selecting the parameters
		//defined by the ValDefs from a term with the given String as its name
        def valDefsToSelect(x:List[ValDef], modification: String):List[Select] = x match {
            case ValDef(a,b,c,d) :: z => {
                val tmp=newTermName(modification); 
                q"$tmp.$b" :: valDefsToSelect(z,modification)} 
            case Nil => Nil
        }
        //updateType
        def updateType(x:List[ValDef], name:Tree, types:List[Tree], newType:String ):List[ValDef] = x match {
            case ValDef(a,b,c,d) :: z => {
                //println(showRaw(c))
                q"class ignoreMe extends $c" match {
                    case q"class ignoreMe extends $name2[..$types2]" if(name.toString==name2.toString && types.toString==types2.toString) => ValDef(a,b,Ident(newTypeName(newType.toString)),d) :: updateType(z,name,types,newType.toString) 
                    case _ => ValDef(a,b,c,d) :: updateType(z,name,types,newType.toString)
                }
            }
            case Nil => Nil
            case _ => x
        }
	def expandFixedPoint(fixed: FixedPoint):List[Tree] = {
		//the additional type parameter
		val fixedType1 = q"type FFunctor"
		//the additional type parameter for the abstract map definition
		val fixedType2 = q"type FFunctor2"
		//the additional type parameter for the fold definition 
		val fixedType3 = q"type FFunctor3"
		//the name for the Fixed Point trait (original name with appended F)
		val newTraitName = newTypeName(fixed.name.toString+"F")
		//the type parameters for the Fixed Point trait
		val newTypeList = fixed.typeParams ++ List(fixedType1)
		//the type parameters of the map definition as references
		val newTypeReferences2 = typeDefsToTypeRefs(fixed.typeParams ++ List(fixedType2))
		//the type parameters of the fold function as references
		val newTypeReferences3 = typeDefsToTypeRefs(fixed.typeParams ++ List(fixedType3))
		//the map function definition
        val maps = q"def map[FFunctor2](g: FFunctor => FFunctor2): $newTraitName[..$newTypeReferences2]"
        val newBodyFixed = List(maps)
		//the new type params of the the original trait
        val extendTypes = typeDefsToTypeRefs(fixed.typeParams ++ List(q"type ${fixed.name}[..${fixed.typeParams}]"))
		//the fold definition 
        val folds = q"def fold[$fixedType3](phi: $newTraitName[..$newTypeReferences3] => ${newTypeReferences3.last}): ${newTypeReferences3.last} = phi(this map (_ fold phi))"
        val newBody = List(folds)
        
		//the goal of all this, the fixed point trait and the original trait that now extends it
        val traitFixed = List(q"trait $newTraitName[..$newTypeList]{..$newBodyFixed}")
		val traitNormal = List(q"trait ${fixed.name}[..${fixed.typeParams}] extends $newTraitName[..$extendTypes]{..$newBody}")
		
		traitFixed ++ traitNormal
		
	}
	//Variant(name: TypeName, typeParams: List[TypeDef], valParams: List[ValDef])
	def expandVariant(variant:Variant, fixed:FixedPoint) = {
	    //the name for the fixed point (traitnameF)
        val newtrait = newTypeName(fixed.name.toString+"F")
		//the name of the original trait (which will now extend the new trait)
        val oldtrait = newTypeName(fixed.name.toString+"")
		//The additional type parameter
		val fixedType1 = q"type FFunctor"
		//The additional type parameter for the map functions
		val fixedType2 = q"type FFunctor2"
		//
		//val fixedType3 = q"type FFunctor3"
		//original type references
		val originalTypes = typeDefsToTypeRefs(variant.typeParams)
		//The new txpe needed for the case class
        val typeRef = typeDefsToTypeRefs(List(q"type $oldtrait[..${variant.typeParams}]"))
		//the name of the case class
        val newName = newTypeName(variant.name.toString+"F")
		//the name of the case class as a Termname, needed for the map(case class) and apply(object) function 
        val newNameTerm = Ident(newTermName(variant.name.toString+"F"))
		//the name of the Object
		val nameTerm = newTermName(variant.name.toString+"")
		//the name of the normal class (and needed for the unapply fuction)
		val oldName = newTypeName(variant.name.toString+"")
		//the type params for the case class
        val updatedTypeParams = variant.typeParams ++List(fixedType1)  //List(q"type FFunctor")//
		//part of the return type (and result) of the map function
        val mapType = typeDefsToTypeRefs(variant.typeParams ++ List(fixedType2)) //List(q"type FFunctor2") //
        //the name of the parameters. devoid of their type, used on multiple occasions
		val paramReferences = valDefsToValRefs(variant.valParams)
		//for the map function it is necessary to apply the function to parameters of the correct type
        val appliedParams = applyDefinedValsOfTypeTo(variant.valParams,typeRef.head,Ident(newTermName("g")))
		//the parameters of the case class need to be updated to the correct type
        val newParams = updateType(variant.valParams,Ident(oldtrait),originalTypes,"FFunctor")
        //the map function for the case classes
		var mapFun = q"def map[FFunctor2](g: FFunctor => FFunctor2): $newtrait[..$mapType] = $newNameTerm[..$mapType]()" //(..$fieldnames)"
        if(paramReferences.length!=0)
			mapFun = q"def map[FFunctor2](g: FFunctor => FFunctor2): $newtrait[..$mapType] = $newNameTerm(..$appliedParams)	"   //${Ident(newName)}(..$appliedParams)" //
        val mapBody = List(mapFun)
		//the type params of the class as references for the apply and unapply functions
	    val typeRefs = typeDefsToTypeRefs(variant.typeParams)
		//the type that the normal class extends if there are parameters
		val temp01 = Ident(newTypeName(variant.name.toString+"F"))// q"$newNameType(..$fieldnames)"
        val extendType = Apply(temp01,paramReferences)//q"$newNameTerm(..$paramReferences)"//Apply(temp01,paramReferences)//q"$temp01(..$paramReferences)"
		//part of the type of the normal class, if there are no parameters
        val extendTypeParams = typeRefs ++ typeRef
		//for the unapply function, the parameters need to be selected 
        val paramsSelect = valDefsToSelect(variant.valParams,"u")
		//the apply function of the object
        var app = q"def apply[..${variant.typeParams}](..${variant.valParams}):$oldtrait[..$typeRefs] = new ${Ident(oldName)}(..$paramReferences)"
        if(variant.valParams.length==0) app = q"def apply[..${variant.typeParams}](..${variant.valParams}):$oldtrait[..$typeRefs] = new ${Ident(oldName)}[..$originalTypes]"
		//the unapply function of the object
		val unapp = q"def unapply[..${variant.typeParams}](u: $oldName[..$typeRefs]):Option[Unit] = Some((..$paramsSelect))"
        val objectBody = List(app) ++ List(unapp)        
		//the purpose of all this, the case class, normal class and its companion object
		val caseClass = q"case class $newName[..$updatedTypeParams](..$newParams) extends $newtrait[..${typeDefsToTypeRefs(updatedTypeParams)}]{..$mapBody}"
		val classClass = if(paramReferences.length<1) q"class $oldName[..${variant.typeParams}](..${variant.valParams}) extends $newName[..$extendTypeParams] with $oldtrait[..${typeDefsToTypeRefs(variant.typeParams)}]"
						 else{
							q"class ${variant.name}[..${variant.typeParams}](..${variant.valParams}) extends ConsF(head,tail) with $oldtrait[..${typeDefsToTypeRefs(variant.typeParams)}]"  match{ // /*
							//q"class ${variant.name}[..${variant.typeParams}](..${variant.valParams}) extends $extendType with $oldtrait[..${typeDefsToTypeRefs(variant.typeParams)}]" match{
								case ClassDef(a,b,c,d) => {
										d match{
											case Template(a2,b2,c2) => ClassDef(a,b,c,Template(extendType :: a2.tail,b2,c2))
										}
									}
							}
							// */
						 } 
		val objectObject = q"object $nameTerm{..$objectBody}"
		//return the three definitions
		List(caseClass) ++ List(classClass) ++ List(objectObject)
    }

  def businessLogic(input: BusinessInput): List[Tree] = {

	val y = input.variants.iterator
	var result = expandFixedPoint(input.fixed)
	while(y.hasNext)
		result = result ++ expandVariant(y.next,input.fixed)
	result
  }

  def createInput(raw: List[Tree]): BusinessInput = {
    val fixed = findFixedPoint(raw)
    BusinessInput(fixed, findVariants(raw,fixed.name))
  }
  
  def createOutput(original: Tree): Tree =
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
        def modify2(x:Tree,original:Tree):Tree = x match {
            case ModuleDef(a,b,templ) => ModuleDef(a,b,modify2(templ,original).asInstanceOf[Template])
            case Template(a,b,list) => Template(a,b,businessLogic(createInput(extractDefList(original))))
            case _ => x
        }  
        
		val res = createOutput(expandees(0))
        val outputs = expandees
		println("?"*50)
		println(res)
	
        c.Expr[Any](Block(List(res), Literal(Constant(()))))
        //c.Expr[Any](res)
		
    }    
}


