package patterns

case class UserDefinedTypeName(name: String)
case class Variable(name: String)
case class ConstructorName(name: String)

sealed trait Type
case object IntType extends Type
case class UserDefinedType(name: UserDefinedTypeName) extends Type

case class ConstructorDefinition(name: ConstructorName, params: Seq[Type])
case class TypeDefinition(name: UserDefinedTypeName, constructors: Seq[ConstructorDefinition])

sealed trait Pattern
case class VariablePattern(variable: Variable) extends Pattern
case class ConstructorPattern(name: ConstructorName, params: Seq[Pattern]) extends Pattern

case class Case(pattern: Pattern, code: Expression)

sealed trait Expression
case class IntExpression(value: Int) extends Expression
case class LetExpression(variable: Variable, value: Expression, in: Expression) extends Expression
case class CallConstructor(name: ConstructorName, params: Seq[Expression]) extends Expression
case class PatternMatch(on: Expression, cases: Seq[Case]) extends Expression
