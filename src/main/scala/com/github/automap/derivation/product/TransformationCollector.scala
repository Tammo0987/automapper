package com.github.automap.derivation.product

import com.github.automap.api.dsl.*

import scala.annotation.tailrec
import scala.quoted.{Expr, Quotes, Type}

object TransformationCollector {

  def collectTransformations[From: Type, To: Type, Q <: Quotes](
      mapperBuilderExpression: Expr[MapperBuilder[From, To]]
  )(using ctx: Q): Seq[Transformation[From, _, Q]] = collect(mapperBuilderExpression, Seq.empty)

  @tailrec
  private def collect[From: Type, To: Type, Q <: Quotes](
      mapperBuilderExpression: Expr[MapperBuilder[From, To]],
      accumulated: Seq[Transformation[From, _, Q]]
  )(using ctx: Q): Seq[Transformation[From, _, Q]] = {
    import ctx.reflect.*

    mapperBuilderExpression match {
      case '{ buildMapper[From, To] } => accumulated
      case '{ withValue[From, To]($nested)[fieldType]($selector, $value) } =>
        val fieldSymbol         = extractFieldSymbolFromSelector(selector)
        val valueTransformation = new Transformation[From, fieldType, Q](fieldSymbol, _ => value)
        collect(nested, accumulated :+ valueTransformation)
      case '{ withComputed[From, To]($nested)[fieldType]($selector, $computation) } =>
        val fieldSymbol = extractFieldSymbolFromSelector(selector)
        val computedTransformation = new Transformation[From, fieldType, Q](
          fieldSymbol,
          from => Expr.betaReduce('{ $computation(${ from }) })
        )
        collect(nested, accumulated :+ computedTransformation)
      case '{ withRenamed[From, To]($nested)[fromField, toField]($toSelector, $fromSelector) } =>
        val fieldSymbol = extractFieldSymbolFromSelector(toSelector)

        val renamedTransformation = new Transformation[From, fromField, Q](
          fieldSymbol,
          from => Expr.betaReduce('{ $fromSelector(${ from }) })
        )

        collect(nested, accumulated :+ renamedTransformation)
      case invalid =>
        report.errorAndAbort(s"Error while collecting transformation: ${invalid.show}")
    }
  }

  private def extractFieldSymbolFromSelector[T, Q <: Quotes](
      selector: Expr[T]
  )(using ctx: Q): ctx.reflect.Symbol = {
    import ctx.reflect.*

    selector.asTerm match {
      case Lambda(_, select @ Select(_, _)) => select.symbol
    }
  }

}
