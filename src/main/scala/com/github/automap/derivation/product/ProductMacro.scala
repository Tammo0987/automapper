package com.github.automap.derivation.product

import com.github.automap.api.Mapper
import com.github.automap.api.dsl.MapperBuilder
import com.github.automap.derivation.product.Projection
import com.github.automap.derivation.product.projections.*

import scala.compiletime.asMatchable
import scala.quoted.*

protected[derivation] class ProductMacro[From: Type, To: Type, Q <: Quotes](using ctx: Q)(
    mapperBuilderExpression: Expr[MapperBuilder[From, To]],
    fromTypeRepr: ctx.reflect.TypeRepr,
    toTypeRepr: ctx.reflect.TypeRepr
) {
  import ctx.reflect.*

  private val projections: Seq[Projection[From, ctx.type]] = Seq(new ByNameAndTypeProjection())

  def deriveMapper(): Expr[Mapper[From, To]] = '{
    new Mapper[From, To] {
      override def map(from: From): To = ${ mapImplementation('{ from }) }
    }
  }

  private def mapImplementation(fromExpression: Expr[From]): Expr[To] = {
    val fromFieldsMap = collectFieldsForTypeRepr(fromTypeRepr)
    val toFieldsMap   = collectFieldsForTypeRepr(toTypeRepr)

    val curriedProjections = projections.map(_.project(fromExpression, fromFieldsMap))

    val constructorArguments = toFieldsMap.map { case (toFieldName, toFieldTypeRepr) =>
      curriedProjections
        .flatMap(_.apply(toFieldName, toFieldTypeRepr))
        .headOption
        .getOrElse(
          report.errorAndAbort(s"No projection is applicable for field: $toFieldName")
        )
    }.toList

    constructInstance(toTypeRepr, constructorArguments).asExprOf[To]
  }

  private def collectFieldsForTypeRepr(typeRepr: TypeRepr): Map[String, TypeRepr] =
    typeRepr.typeSymbol.caseFields.map(fieldSymbol => fieldSymbol.name -> typeRepr.memberType(fieldSymbol)).toMap

  private def constructInstance(
      instanceTypeRepr: TypeRepr,
      arguments: List[Term]
  ): Term = {
    Select.overloaded(
      Ref(instanceTypeRepr.typeSymbol.companionModule),
      "apply",
      List.empty,
      arguments,
      instanceTypeRepr
    )
  }

}
