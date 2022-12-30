package appolaris.match_deconstruct

import scala.reflect.ClassTag

type Rule[TRes] = PartialFunction[Any, TRes]

case object Hole
case class TypeCheck(self: ClassTag[?])
case class Obj(self: Any)
case class Extractor(self: ClassTag[?], params: Seq[PatternStructure])

type PatternStructure = Hole.type | AnyVal | String | TypeCheck | Obj | Extractor

case class RuleWithStructure[TRes](rule: Rule[TRes], structure: PatternStructure)
