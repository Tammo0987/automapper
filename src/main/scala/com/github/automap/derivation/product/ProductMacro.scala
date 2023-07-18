package com.github.automap.derivation.product

import com.github.automap.api.Mapper
import com.github.automap.api.dsl.MapperBuilder

import scala.quoted.{Expr, Quotes, Type}

protected[derivation] class ProductMacro[From: Type, To: Type, Q <: Quotes](using ctx: Q)(
    mapperBuilderExpression: Expr[MapperBuilder[From, To]],
    fromTypeRepr: ctx.reflect.TypeRepr,
    toTypeRepr: ctx.reflect.TypeRepr
) {
  import ctx.reflect.*

  private sealed trait Projection {
    type Error  = String
    type Result = Either[Error, Option[Term]]

    def project(
        toFieldName: String,
        toFieldType: TypeRepr,
        fromExpression: Expr[From],
        fromFields: Map[String, TypeRepr]
    ): Result

    def priority: Int
  }

  private class ByNameAndTypeProjection(override val priority: Int = 1) extends Projection {
    override def project(
        toFieldName: String,
        toFieldType: TypeRepr,
        fromExpression: Expr[From],
        fromFields: Map[String, TypeRepr]
    ): Result = {
      fromFields
        .get(toFieldName)
        .filter(_ =:= toFieldType)
        .toRight(s"No Field with same type and name found for: $toFieldName")
        .map(_ => Some(Select.unique(fromExpression.asTerm, toFieldName)))
    }
  }

  // TODO move to companion object?
  private val projections: Seq[Projection] = Seq(new ByNameAndTypeProjection())

  def deriveMapper(): Expr[Mapper[From, To]] = '{
    new Mapper[From, To] {
      override def map(from: From): To = ${ mapImplementation('{ from }) }
    }
  }

  private def mapImplementation(fromExpression: Expr[From]): Expr[To] = {
    val fromFieldsMap = collectFieldsForTypeRepr(fromTypeRepr)
    val toFieldsMap   = collectFieldsForTypeRepr(toTypeRepr)

    ???
  }

  private def collectFieldsForTypeRepr(typeRepr: TypeRepr): Map[String, TypeRepr] =
    typeRepr.typeSymbol.caseFields.map(fieldSymbol => fieldSymbol.name -> typeRepr.memberType(fieldSymbol)).toMap

}
