package com.github.automap.derivation

import com.github.automap.api.dsl.MapperBuilder
import com.github.automap.api.Mapper
import com.github.automap.derivation.product.ProductMacro

import scala.quoted.{Expr, Quotes, Type}

object Derivation {

  inline def deriveMapperMacro[From, To](
      inline mapperBuilder: MapperBuilder[From, To]
  ): Mapper[From, To] = ${ deriveMapperMacroImplementation('{ mapperBuilder }) }

  private def deriveMapperMacroImplementation[From: Type, To: Type](
      mapperBuilderExpression: Expr[MapperBuilder[From, To]]
  )(using ctx: Quotes): Expr[Mapper[From, To]] = {
    import ctx.reflect.*

    val fromTypeRepr = TypeRepr.of[From]
    val toTypeRepr   = TypeRepr.of[To]

    val fromFlags = fromTypeRepr.typeSymbol.flags
    val toFlags   = toTypeRepr.typeSymbol.flags

    if (fromFlags.isCaseClass && toFlags.isCaseClass) {
      new ProductMacro[From, To, ctx.type](mapperBuilderExpression, fromTypeRepr, toTypeRepr).deriveMapper()
    } else {
      report.errorAndAbort("Unsupported Mapping Types")
    }
  }

  extension [Q <: Quotes](using ctx: Q)(flags: ctx.reflect.Flags) {
    def isCaseClass: Boolean = flags.is(ctx.reflect.Flags.Case)
  }

}
