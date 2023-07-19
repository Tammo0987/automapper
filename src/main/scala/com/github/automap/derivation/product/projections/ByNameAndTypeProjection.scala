package com.github.automap.derivation.product.projections

import com.github.automap.derivation.product.Projection

import scala.quoted.{Expr, Quotes}

class ByNameAndTypeProjection[From, Q <: Quotes](using override val ctx: Q)
    extends Projection[From, Q] {
  import ctx.reflect.*

  override def project(
      fromExpression: Expr[From],
      fromFields: Map[String, TypeRepr]
  )(toFieldName: String, toFieldType: TypeRepr): Option[Term] =
    fromFields
      .get(toFieldName)
      .filter(_ =:= toFieldType)
      .map(_ => Select.unique(fromExpression.asTerm, toFieldName))
}
