package com.github.automap.derivation.product

import scala.quoted.{Expr, Quotes}

class Transformation[From, T, Q <: Quotes](using ctx: Q)(
    symbol: ctx.reflect.Symbol,
    value: Expr[From] => Expr[T]
)
