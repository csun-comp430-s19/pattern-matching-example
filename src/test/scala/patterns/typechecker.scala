package patterns

import org.scalatest.FlatSpec

class TestTypechecker extends FlatSpec {
  import Typechecker.typecheck

  "The typechecker" should "handle integer literals" in {
    assertResult(IntType) {
      typecheck(Program(Seq(), IntExpression(5)))
    }
  }

  it should "reject type definitions with duplicate constructors" in {
    assertThrows[TypeErrorException] {
      typecheck(Program(Seq(
        TypeDefinition(
          UserDefinedTypeName("Foo"),
          Seq(
            ConstructorDefinition(ConstructorName("Bar"), Seq()),
            ConstructorDefinition(ConstructorName("Bar"), Seq())))),
          IntExpression(5)))
    }
  }

  it should "reject type definitions with duplicate constructors across multiple definitions" in {
    assertThrows[TypeErrorException] {
      typecheck(Program(Seq(
        TypeDefinition(
          UserDefinedTypeName("Foo"),
          Seq(ConstructorDefinition(ConstructorName("Bar"), Seq()))),
        TypeDefinition(
          UserDefinedTypeName("Baz"),
          Seq(ConstructorDefinition(ConstructorName("Bar"), Seq())))),
        IntExpression(5)))
    }
  }

  it should "reject type definitions with duplicate type names" in {
    assertThrows[TypeErrorException] {
      typecheck(Program(Seq(
        TypeDefinition(
          UserDefinedTypeName("Foo"),
          Seq(ConstructorDefinition(ConstructorName("Bar"), Seq()))),
        TypeDefinition(
          UserDefinedTypeName("Foo"),
          Seq(ConstructorDefinition(ConstructorName("Baz"), Seq())))),
        IntExpression(5)))
    }
  }

  it should "reject a variable usage out of scope" in {
    assertThrows[TypeErrorException] {
      typecheck(Program(Seq(),
        VariableExpression(Variable("x"))))
    }
  }

  it should "accept an in-scope variable usage" in {
    assertResult(IntType) {
      typecheck(Program(Seq(),
        LetExpression(
          Variable("x"),
          IntExpression(5),
          VariableExpression(Variable("x")))))
    }
  }

  it should "accept a well-typed constructor usage" in {
    assertResult(UserDefinedType(UserDefinedTypeName("Foo"))) {
      typecheck(Program(Seq(
        TypeDefinition(
          UserDefinedTypeName("Foo"),
          Seq(ConstructorDefinition(ConstructorName("Bar"), Seq(IntType))))),
        CallConstructor(ConstructorName("Bar"), Seq(IntExpression(5)))))
    }
  }

  it should "reject an ill-typed constructor usage - arity mismatch" in {
    assertThrows[TypeErrorException] {
      typecheck(Program(Seq(
        TypeDefinition(
          UserDefinedTypeName("Foo"),
          Seq(ConstructorDefinition(ConstructorName("Bar"), Seq(IntType))))),
        CallConstructor(ConstructorName("Bar"), Seq(IntExpression(5), IntExpression(6)))))
    }
  }

  it should "reject an ill-typed constructor usage - type mismatch" in {
    assertThrows[TypeErrorException] {
      typecheck(Program(Seq(
        TypeDefinition(
          UserDefinedTypeName("Foo"),
          Seq(ConstructorDefinition(ConstructorName("Bar"), Seq(IntType))))),
        CallConstructor(ConstructorName("Bar"),
          Seq(CallConstructor(ConstructorName("Bar"), Seq(IntExpression(5)))))))
    }
  }

  // type IntList = Nil | Cons(Int, IntList)
  val INT_LIST_NAME = UserDefinedTypeName("IntList")
  val NIL_NAME = ConstructorName("Nil")
  val CONS_NAME = ConstructorName("Cons")
  val INT_LIST_DEFINITION = TypeDefinition(
    INT_LIST_NAME,
    Seq(
      ConstructorDefinition(NIL_NAME, Seq()),
      ConstructorDefinition(CONS_NAME, Seq(IntType, UserDefinedType(INT_LIST_NAME)))))

  it should "accept a well-typed pattern match - catch-all pattern" in {
    assertResult(UserDefinedType(INT_LIST_NAME)) {
      typecheck(Program(Seq(INT_LIST_DEFINITION),
        PatternMatch(
          CallConstructor(NIL_NAME, Seq()),
          Seq(
            Case(VariablePattern(Variable("x")), VariableExpression(Variable("x")))))))
    }
  }
} // TestTypechecker
