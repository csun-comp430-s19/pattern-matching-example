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
} // TestTypechecker
