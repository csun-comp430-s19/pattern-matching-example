package patterns

case class TypeErrorException(msg: String) extends Exception(msg)

case class ConstructorInfo(
  correspondingUserType: UserDefinedTypeName,
  expectedTypes: Seq[Type])

object Typechecker {
  def apply(prog: Program): Typechecker = {
    def addTypedef(
      constructorsForType: Map[UserDefinedTypeName, Set[ConstructorName]],
      constructorInfo: Map[ConstructorName, ConstructorInfo],
      typedef: TypeDefinition): (Map[UserDefinedTypeName, Set[ConstructorName]], Map[ConstructorName, ConstructorInfo]) = {

      if (constructorsForType.contains(typedef.name)) {
        throw TypeErrorException("Duplicate type name: " + typedef.name)
      }
      val constructorsSeq = typedef.constructors.map(_.name)
      val constructorsSet = constructorsSeq.toSet
      if (constructorsSeq.size != constructorsSet.size) {
        throw TypeErrorException("Duplicate constructor name within typedef: " + typedef.name)
      }

      val newConstructorsForType = constructorsForType + (typedef.name -> constructorsSet)
      val newConstructorInfo = typedef.constructors.foldLeft(constructorInfo)((res, cur) => {
        if (res.contains(cur.name)) {
          throw TypeErrorException("Duplicate constructor name: " + cur.name)
        }
        res + (cur.name -> ConstructorInfo(typedef.name, cur.params))
      })
      (newConstructorsForType, newConstructorInfo)
    }

    val initial: (Map[UserDefinedTypeName, Set[ConstructorName]], Map[ConstructorName, ConstructorInfo]) =
      (Map(), Map())
    val (finalConstructorsForType, finalConstructorInfo) = prog.typedefs.foldLeft(initial)((res, cur) =>
      addTypedef(res._1, res._2, cur))
    new Typechecker(finalConstructorsForType, finalConstructorInfo)
  } // apply

  def typecheck(prog: Program): Type = {
    apply(prog).typeof(prog.body, Map())
  } // typecheck
} // Typechecker

class Typechecker(
  val constructorsForType: Map[UserDefinedTypeName, Set[ConstructorName]],
  val constructorInfo: Map[ConstructorName, ConstructorInfo]) {

  def getConstructorInfo(name: ConstructorName, numParams: Int): ConstructorInfo = {
    val result = constructorInfo.getOrElse(
      name,
      throw TypeErrorException("Unknown constructor name: " + name))
    if (result.expectedTypes.size != numParams) {
      throw TypeErrorException("Constructor arity mismatch: " + name)
    }
    result
  } // getConstructorInfo

  def joinSeqs(seq1: Seq[MatchValue], seq2: Seq[MatchValue]): Seq[MatchValue] = {
    assert(seq1.size == seq2.size)
    seq1.zip(seq2).map(
      { case (first, second) => first.join(second) })
  } // joinSeqs

  def allHandled(forType: UserDefinedTypeName, map: Map[ConstructorName, Seq[MatchValue]]): Boolean = {
    val allConstructors = constructorsForType(forType)
    map.keySet == allConstructors && map.values.forall(_.forall(_.isMatchAll))
  } // allHandled

  def joinMaps(
    map1: Map[ConstructorName, Seq[MatchValue]],
    map2: Map[ConstructorName, Seq[MatchValue]]): Map[ConstructorName, Seq[MatchValue]] = {
    val onlyInMap1 = map1.filterKeys(k => !map2.contains(k))
    val onlyInMap2 = map2.filterKeys(k => !map1.contains(k))
    val inBoth = map1.keySet.intersect(map2.keySet)

    inBoth.map(k => (k, joinSeqs(map1(k), map2(k)))).toMap ++ onlyInMap1 ++ onlyInMap2
  } // joinMaps

  sealed trait MatchValue {
    def join(other: MatchValue): MatchValue
    def isMatchAll: Boolean
  } // MatchValue

  case object MatchAll extends MatchValue {
    def join(other: MatchValue): MatchValue = {
      throw TypeErrorException("Pattern will never be reached")
    } // join
    def isMatchAll: Boolean = true
  } // MatchAll

  case object MatchNone extends MatchValue {
    def join(other: MatchValue): MatchValue = other
    def isMatchAll: Boolean = false
  } // MatchNone

  case class MatchSome(
    forType: UserDefinedTypeName,
    matchedSoFar: Map[ConstructorName, Seq[MatchValue]]) extends MatchValue {

    def join(other: MatchValue): MatchValue = {
      other match {
        case MatchAll => MatchAll
        case MatchNone => this
        case MatchSome(otherForType, otherMatchedSoFar) => {
          if (forType != otherForType) {
            throw TypeErrorException("Type mismatch in pattern")
          }
          val resultMap = joinMaps(matchedSoFar, otherMatchedSoFar)
          if (allHandled(forType, resultMap)) {
            MatchAll
          } else {
            MatchSome(forType, resultMap)
          }
        }
      }
    } // join

    def isMatchAll: Boolean = false
  } // MatchSome

  def patternToMatchValue(pattern: Pattern): MatchValue = {
    pattern match {
      case VariablePattern(_) => MatchAll
      case ConstructorPattern(name, params) => {
        val ConstructorInfo(userType, expectedTypes) = getConstructorInfo(name, params.size)
        MatchSome(
          userType,
          Map(name -> params.map(patternToMatchValue)))
      }
    }
  } // patternToMatchValue

  def patternVariables(contextType: Type, pattern: Pattern): Map[Variable, Type] = {
    pattern match {
      case VariablePattern(variable) => Map(variable -> contextType)
      case ConstructorPattern(name, params) => {
        contextType match {
          case UserDefinedType(contextTypeName) => {
            val ConstructorInfo(expectedUserType, expectedTypes) = getConstructorInfo(name, params.size)
            if (contextTypeName != expectedUserType) {
              throw TypeErrorException("Expected: " + expectedUserType + "; received: " + contextTypeName)
            }
            assert(params.size == expectedTypes.size)
            expectedTypes.zip(params).foldLeft(Map[Variable, Type]())((res, cur) => {
              val curResult = patternVariables(cur._1, cur._2)
              if (curResult.keySet.intersect(res.keySet).nonEmpty) {
                throw TypeErrorException("Duplicate variable introduced in pattern")
              }
              res ++ curResult
            })
          }
          case _ => throw TypeErrorException("Expected user type; got: " + contextType)
        }
      }
    }
  } // patternVariables

  def typeof(exp: Expression, env: Map[Variable, Type]): Type = {
    exp match {
      case IntExpression(_) => IntType
      case VariableExpression(variable) => {
        env.getOrElse(variable,
          throw TypeErrorException("Variable not in scope: " + variable))
      }
      case LetExpression(variable, value, in) => {
        val valueType = typeof(value, env)
        typeof(in, env + (variable -> valueType))
      }
      case CallConstructor(name, params) => {
        val ConstructorInfo(typeName, expectedTypes) = getConstructorInfo(name, params.size)
        if (params.map(typeof(_, env)) != expectedTypes) {
          throw TypeErrorException("Constructor parameter type mismatch: " + name)
        }
        UserDefinedType(typeName)
      }
      case PatternMatch(on, cases) => {
        typeof(on, env) match {
          case userType@UserDefinedType(_) => {
            val (Some(finalReturn), finalMatch) =
              cases.foldLeft((None: Option[Type], MatchNone: MatchValue))((res, cur) => {
                val (returnType, curMatch) = res
                val Case(curPattern, curExp) = cur
                val nextMatch = curMatch.join(patternToMatchValue(curPattern))
                val addVariables = patternVariables(userType, curPattern)
                val curReturnType = typeof(curExp, env ++ addVariables)
                val nextReturnType = returnType match {
                  case None | Some(`curReturnType`) => Some(curReturnType)
                  case _ => throw TypeErrorException("Not all cases return same type")
                }
                (nextReturnType, nextMatch)
              })
            if (!finalMatch.isMatchAll) {
              throw TypeErrorException("Non-exhaustive pattern match")
            }
            finalReturn
          }
          case other => {
            throw TypeErrorException("Can only pattern match on user types; found: " + other)
          }
        }
      }
    }
  } // typeof
} // Typechecker
