package com.github.automap.derivation.product

import scala.quoted.{Expr, Quotes, quotes}

trait Projection[From, Q <: Quotes] {

  val ctx: Q
  import ctx.reflect.*

  def project(fromExpression: Expr[From], fromFields: Map[String, TypeRepr])(
      toFieldName: String,
      toFieldType: TypeRepr
  ): Option[Term]

}
