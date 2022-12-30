package appolaris.match_deconstruct

import appolaris.match_deconstruct.{Extractor, Hole, Obj, PatternApi, RuleWithStructure, TypeCheck}

import scala.collection.immutable.List
import scala.reflect.{ClassTag, classTag}

class MacroTest extends munit.FunSuite {

  case class Xxx(i: Int, s: String)
  case class Yyy(i: Int, x: Xxx)
  case class VarArgs(i: Int*)
  case class NonGeneric(i: Int)
  case class Generic[T](i: T)

  // Useful for tracing
//  test("Inspect") {
//    inspect ({
//      case x:Option[?] => 1
//    }: PartialFunction[Any, Any])
//  }

  private def pattern[TRes](xs: List[RuleWithStructure[TRes]]) = xs match {
    case List(RuleWithStructure(_, str)) => str
    case _ => fail("Expected list with single result", clues(xs))
  }

  test("Default case") {
    // Wildcard()
    val res = PatternApi.deconstruct {
      case _ => 1
    }
    assertEquals(pattern(res), Hole)
  }

  test("Simple non-generic case class destructuring, without free vars") {
    // TypedOrTest(Unapply(Select(Ident("NonGeneric"), "unapply"), Nil, List(Literal(IntConstant(1)))), Inferred())
    val res = PatternApi.deconstruct {
      case NonGeneric(1) => 1
    }
    assertEquals(pattern(res), Extractor(classTag[NonGeneric.type], List(1)))
  }

  test("Simple non-generic case class destructuring, with free var binding") {
    // TypedOrTest(Unapply(Select(Ident("NonGeneric"), "unapply"), Nil, List(Bind("x", Wildcard()))), Inferred())
    val res = PatternApi.deconstruct {
      case NonGeneric(x) => x
    }
    assertEquals(pattern(res), Extractor(classTag[NonGeneric.type], List(Hole)))
  }

  test("Case class with varargs destructuring") {
    // TypedOrTest(Unapply(Select(Ident("Sss"), "unapplySeq"), Nil, List(Literal(IntConstant(1)), Literal(IntConstant(2)))), Inferred())
    val res = PatternApi.deconstruct {
      case VarArgs(1,2) => 1
    }
    assertEquals(pattern(res), Extractor(classTag[VarArgs.type], List(1, 2)))
  }

  test("Non-generic nested case classes") {
    // TypedOrTest(Unapply(Select(Ident("Yyy"), "unapply"), Nil, List(Literal(IntConstant(1)), Unapply(Select(Ident("Xxx"), "unapply"), Nil, List(Bind("i", Wildcard()), Literal(StringConstant("S")))))), Inferred())
    val res = PatternApi.deconstruct {
      case Yyy(1, Xxx(i, "S")) => 33
    }
    assertEquals(pattern(res),
      Extractor(classTag[Yyy.type], List(
        1,
        Extractor(classTag[Xxx.type], List(Hole, "S"))
      ))
    )
  }

  test("Generic case class") {
    // TypedOrTest(Unapply(TypeApply(Select(Ident("Some"), "unapply"), List(Inferred())), Nil, List(Bind("x", Wildcard()))), Inferred())
    val res = PatternApi.deconstruct {
      case Some(x) => x
    }
    assertEquals(pattern(res), Extractor(classTag[Some.type], List(Hole)))
  }

  test("Nested generic case classes") {
    // TypedOrTest(Unapply(TypeApply(Select(Ident("Some"), "unapply"), List(Inferred())), Nil, List(TypedOrTest(Unapply(TypeApply(Select(Ident("Some"), "unapply"), List(Inferred())), Nil, List(Bind("x", Wildcard()))), Inferred()))), Inferred())
    val res = PatternApi.deconstruct {
      case Some(Some(x)) => x
    }
    assertEquals(pattern(res), Extractor(classTag[Some.type], List(Extractor(classTag[Some.type], List(Hole)))))
  }

  test("Object") {
    // Ident("None")
    val res = PatternApi.deconstruct {
      case None => 1
    }
    assertEquals(pattern(res), Obj(None))
  }

  test("Wildcard type test") {
    // Typed(Wildcard(), TypeIdent("Int"))
    val res = PatternApi.deconstruct {
      case _: Int => 1
    }
    assertEquals(pattern(res), TypeCheck(classTag[Int]))
  }

  test("Binding type test") {
    // Bind("x", Typed(Wildcard(), TypeIdent("Xxx")))
    val res = PatternApi.deconstruct {
      case x:Xxx => 1
    }
    assertEquals(pattern(res), TypeCheck(classTag[Xxx]))
  }

  test("Binding type test (generic case)") {
    // Bind("x", Typed(Wildcard(), Applied(TypeIdent("Option"), List(TypeBind(_$1, TypeBoundsTree(Inferred(), Inferred()))))))
    val res = PatternApi.deconstruct {
      case x:Option[?] => 1
    }
    assertEquals(pattern(res), TypeCheck(classTag[Option[?]]))
  }

  test("Const") {
    // Literal(IntConstant(1))
    val res = PatternApi.deconstruct {
      case 1 => 2
    }
    assertEquals(pattern(res), 1)
  }

  test("Const with binding") {
    // Bind("i", Literal(IntConstant(1)))
    val res = PatternApi.deconstruct {
      case i @ 1 => 2
    }
    assertEquals(pattern(res), 1)
  }

  test("Apply resulting function") {
    val i = 872
    val res = PatternApi.deconstruct {
      case NonGeneric(x) => s"Hubba $i $x"
      case Some(x) => s"Bubba $i $x"
    }

    assertEquals(res(0).rule(NonGeneric(1)), "Hubba 872 1")
    assertEquals(res(1).rule(Some("x")), "Bubba 872 x")
  }
}

