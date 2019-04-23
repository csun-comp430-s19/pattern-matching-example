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

  it should "accept a well-typed pattern match - all cases stated at toplevel" in {
    assertResult(IntType) {
      typecheck(Program(Seq(INT_LIST_DEFINITION),
        PatternMatch(
          CallConstructor(NIL_NAME, Seq()),
          Seq(
            Case(
              // Cons(head, tail) => head
              ConstructorPattern(CONS_NAME, Seq(VariablePattern(Variable("head")), VariablePattern(Variable("tail")))),
              VariableExpression(Variable("head"))),
            Case(
              // Nil => 0
              ConstructorPattern(NIL_NAME, Seq()),
              IntExpression(0))))))
    }
  }

  it should "accept a well-typed pattern match - specific followed by catch-all at toplevel" in {
    assertResult(IntType) {
      typecheck(Program(Seq(INT_LIST_DEFINITION),
        PatternMatch(
          CallConstructor(NIL_NAME, Seq()),
          Seq(
            Case(
              // Cons(head, tail) => head
              ConstructorPattern(CONS_NAME, Seq(VariablePattern(Variable("head")), VariablePattern(Variable("tail")))),
              VariableExpression(Variable("head"))),
            Case(
              // x => 0
              VariablePattern(Variable("x")),
              IntExpression(0))))))
    }
  }

  it should "reject an ill-typed pattern match: different types on different arms" in {
    assertThrows[TypeErrorException] {
      typecheck(Program(Seq(INT_LIST_DEFINITION),
        PatternMatch(
          CallConstructor(NIL_NAME, Seq()),
          Seq(
            Case(
              // Cons(head, tail) => tail
              ConstructorPattern(CONS_NAME, Seq(VariablePattern(Variable("head")), VariablePattern(Variable("tail")))),
              VariableExpression(Variable("tail"))),
            Case(
              // Nil => 0
              ConstructorPattern(NIL_NAME, Seq()),
              IntExpression(0))))))
    }
  }

  it should "reject an ill-typed pattern match: dead code due to catch-all" in {
    assertThrows[TypeErrorException] {
      typecheck(Program(Seq(INT_LIST_DEFINITION),
        PatternMatch(
          CallConstructor(NIL_NAME, Seq()),
          Seq(
            Case(
              // x => x
              VariablePattern(Variable("x")),
              VariableExpression(Variable("x"))),
            Case(
              // Nil => 0
              ConstructorPattern(NIL_NAME, Seq()),
              IntExpression(0))))))
    }
  }

  it should "reject an ill-typed pattern match: dead code due to duplicate pattern" in {
    assertThrows[TypeErrorException] {
      typecheck(Program(Seq(INT_LIST_DEFINITION),
        PatternMatch(
          CallConstructor(NIL_NAME, Seq()),
          Seq(
            Case(
              // Cons(head, tail) => head
              ConstructorPattern(CONS_NAME, Seq(VariablePattern(Variable("head")), VariablePattern(Variable("tail")))),
              VariableExpression(Variable("head"))),
            Case(
              // Cons(head, tail) => head
              ConstructorPattern(CONS_NAME, Seq(VariablePattern(Variable("head")), VariablePattern(Variable("tail")))),
              VariableExpression(Variable("head"))),
            Case(
              // Nil => 0
              ConstructorPattern(NIL_NAME, Seq()),
              IntExpression(0))))))
    }
  }

  it should "reject an ill-typed pattern match: non-exhaustive pattern match at toplevel" in {
    assertThrows[TypeErrorException] {
      typecheck(Program(Seq(INT_LIST_DEFINITION),
        PatternMatch(
          CallConstructor(NIL_NAME, Seq()),
          Seq(
            Case(
              // Cons(head, tail) => head
              ConstructorPattern(CONS_NAME, Seq(VariablePattern(Variable("head")), VariablePattern(Variable("tail")))),
              VariableExpression(Variable("head")))))))
    }
  }

  it should "reject an ill-typed pattern match: type error in pattern - wrong type" in {
    assertThrows[TypeErrorException] {
      typecheck(Program(Seq(INT_LIST_DEFINITION),
        PatternMatch(
          CallConstructor(NIL_NAME, Seq()),
          Seq(
            Case(
              // Cons(Cons(head, tail1), tail2) => tail2
              ConstructorPattern(
                CONS_NAME,
                Seq(
                  ConstructorPattern(
                    CONS_NAME,
                    Seq(
                      VariablePattern(Variable("head")),
                      VariablePattern(Variable("tail1")))),
                  VariablePattern(Variable("tail2")))),
              VariableExpression(Variable("tail2")))))))
    }
  }

  it should "reject an ill-typed pattern match: type error in pattern - arity mismatch" in {
    assertThrows[TypeErrorException] {
      typecheck(Program(Seq(INT_LIST_DEFINITION),
        PatternMatch(
          CallConstructor(NIL_NAME, Seq()),
          Seq(
            Case(
              // Cons(head, tail1, tail2) => head
              ConstructorPattern(
                CONS_NAME,
                Seq(
                  VariablePattern(Variable("head")),
                  VariablePattern(Variable("tail1")),
                  VariablePattern(Variable("tail2")))),
              VariableExpression(Variable("head"))),
            Case(
              // Nil => 0
              ConstructorPattern(NIL_NAME, Seq()),
              IntExpression(0))))))
    }
  }

  it should "accept a well-typed pattern match: nested patterns - catch-all" in {
    assertResult(IntType) {
      typecheck(Program(Seq(INT_LIST_DEFINITION),
        PatternMatch(
          CallConstructor(NIL_NAME, Seq()),
          Seq(
            Case(
              // Cons(head1, Cons(head2, tail)) => 0
              ConstructorPattern(
                CONS_NAME,
                Seq(
                  VariablePattern(Variable("head1")),
                  ConstructorPattern(
                    CONS_NAME,
                    Seq(
                      VariablePattern(Variable("head2")),
                      VariablePattern(Variable("tail")))))),
              IntExpression(0)),
            Case(
              // Cons(head, tail) => 1
              ConstructorPattern(
                CONS_NAME,
                Seq(VariablePattern(Variable("head")), VariablePattern(Variable("tail")))),
              IntExpression(1)),
            Case(
              // x => 2
              VariablePattern(Variable("x")),
              IntExpression(2))))))
    }
  }
} // TestTypechecker
