package appolaris.match_deconstruct

import scala.annotation.tailrec
import scala.quoted.*
import scala.reflect.ClassTag

// https://github.com/lampepfl/dotty/blob/246b60221da69682a2fbf815d23a3c08f7f40af0/compiler/src/scala/quoted/runtime/impl/QuotesImpl.scala

object PatternApi {
  inline def deconstruct[TVal, TRes](inline expr: PartialFunction[TVal, TRes]): List[RuleWithStructure[TRes]] =
    ${deconstructImpl('expr)}

  // TVal == Any ?
  private def deconstructImpl[TVal, TRes](expr: Expr[PartialFunction[TVal, TRes]])(using Type[TVal], Type[TRes])(using Quotes): Expr[List[RuleWithStructure[TRes]]] = {
    import quotes.reflect.*
    val term = expr.asTerm
//    println(expr.show)
//    println(term.show(using Printer.TreeStructure))

    val resExprs: List[Expr[RuleWithStructure[TRes]]] = term match {
      case block @ Inlined(None, Nil, Block(
        List(defDef @ DefDef(defName, defParams, defTypeTree, Some(Match(matchParam, cases)))),
        closure
      )) =>
        // Split one partial function with N cases into N partial cases of 1 case each
        cases.map(c => {
          val newRuleExpr = Inlined(None, Nil, Block(
            List(DefDef.copy(defDef)(defName, defParams, defTypeTree, Some(Match(matchParam, List(c))))),
            closure
          )).asExprOf[Rule[TRes]]

          // Get structure of the pattern
          val ruleStructure = getCaseStructure(c)
          '{ RuleWithStructure(${newRuleExpr}, ${ruleStructure}) }
        })
//        println("cases:")
//        newAsts.map(x => x.show(using Printer.TreeStructure)).foreach(println)
      case _ => report.errorAndAbort("Unexpected code structure. Expected pattern-matching anonymous function ({ case p1 => b1 ... case pn => bn })")
    }
    Expr.ofList(resExprs)
  }

  private def getCaseStructure(using Quotes)(caseClause: quotes.reflect.Tree): Expr[PatternStructure] = {
    import quotes.reflect.*
    caseClause match {
      case CaseDef(pattern, _, _) => getPatternStructure(pattern)
      case _ => report.errorAndAbort("Unexpected code structure for the case clause.")
    }
  }

  // todo - check instance extractors
  @tailrec
  private def getPatternStructure(using Quotes)(pattern: quotes.reflect.Tree): Expr[PatternStructure] = {
    import quotes.reflect.*
    //    println("case: " + unapply.show(using Printer.TreeStructure))
    pattern match {
      // Type test (Typed extends TypedOrTest so is tested first)
      case Typed(_, typeIdent) => getTypeCheckPatternStructure(typeIdent)

      // Stripping "TypedOrTest" wrapper
      case TypedOrTest(expr, _) => getPatternStructure(expr)

      // Ignore Bind (we do not include binding info in the resulting structure)
      case Bind(_, expr) => getPatternStructure(expr)

      // Wildcard means (unused) free variable, we return Hole for them
      case Wildcard() => '{ Hole }

      // Object
      case term @ Ident(_) => '{ Obj(${term.asExpr}) }

      // Constants
      case ExtractConstExpr(c) => c

      // Constructor/extractor patterns (non-generic)
      case Unapply(Select(ident, "unapply"), _, params:List[?]) => getUnapplyPatternStructure(ident, params)
      case Unapply(Select(ident, "unapplySeq"), _, params:List[?]) => getUnapplyPatternStructure(ident, params)

      // Constructor/extractor patterns (generic)
      case Unapply(TypeApply(Select(ident, "unapply"), List(Inferred())), _, params:List[?]) => getUnapplyPatternStructure(ident, params)
      case Unapply(TypeApply(Select(ident, "unapplySeq"), List(Inferred())), _, params:List[?]) => getUnapplyPatternStructure(ident, params)

      case _ => report.errorAndAbort(
        "Unexpected code structure for the pattern.\n" +
        "Expected constructor, extractor, or constant pattern (possibly with vars and bindings).\n" +
        "Unexpected expression (or subpart of the expression) was:\n" +
        pattern.show(using Printer.TreeStructure))
    }
  }

  private def getTypeCheckPatternStructure(using Quotes)(typeTree: quotes.reflect.TypeTree): Expr[TypeCheck] = {
    //    trace(s"found unapply: $qualifier")
    val ctexpr = getTypeTreeClassTagExpr(typeTree)
    '{ TypeCheck(${ctexpr}) }
  }

  private def getUnapplyPatternStructure(using Quotes)(ident: quotes.reflect.Term, params: List[quotes.reflect.Tree]): Expr[Extractor] = {
//    trace(s"found unapply: $qualifier")
    val ctexpr = getTermClassTagExpr(ident)
    val pp = params.map(getPatternStructure)
    '{ Extractor(${ctexpr}, ${Expr.ofList(pp)}) }
  }

  private def getTypeTreeClassTagExpr(using Quotes)(typeTree: quotes.reflect.TypeTree): Expr[ClassTag[?]] = {
    val tip: Type[?] = typeTree.tpe.asType
    tip match {
      case '[t] => getClassTagExpr[t]
    }
  }

  private def getTermClassTagExpr(using Quotes)(term: quotes.reflect.Term): Expr[ClassTag[?]] = {
    val tip: Type[?] = term.tpe.asType
    tip match {
      case '[t] => getClassTagExpr[t]
    }
  }

  private def getClassTagExpr[T](using Type[T], Quotes): Expr[ClassTag[T]] = {
    import quotes.reflect._
    Expr.summon[ClassTag[T]] match {
      case Some(ct) => ct
      case None =>
        report.error(
          s"Unable to find a ClassTag for type ${Type.show[T]}",
          Position.ofMacroExpansion
        )
        throw new Exception("Error when applying macro")
    }
  }

  private object ExtractConstExpr {
    def unapply(using Quotes)(lit: quotes.reflect.Tree): Option[Expr[AnyVal | String]] = {
      import quotes.reflect._
      lit match {
        case Literal(c) => c match {
          case IntConstant(x) => Some(Expr(x))
          case StringConstant(x) => Some(Expr(x))
          case _ => None
        }
        case _ => None
      }
    }
  }
}