package com.github.automap.api

import com.github.automap.derivation.Derivation

object dsl {

  // Mapper creation dsl

  opaque type MapperBuilder[From, _] = From

  inline def deriveMapper[From, To]: Mapper[From, To] =
    buildMapper[From, To].build

  def buildMapper[From, To]: MapperBuilder[From, To] = runtimeError()

  extension [From, To](self: MapperBuilder[From, To]) {
    def withValue[Field](
        selector: To => Field,
        value: Field
    ): MapperBuilder[From, To] = runtimeError()

    def withComputed[Field](
        selector: To => Field,
        computation: From => Field
    ): MapperBuilder[From, To] = runtimeError()

    def withRenamed[FromField, ToField](
        toSelector: To => ToField,
        fromSelector: From => FromField
    ): MapperBuilder[From, To] = runtimeError()
  }

  extension [From, To](inline self: MapperBuilder[From, To]) {
    inline def build: Mapper[From, To] = Derivation.deriveMapperMacro(self)
  }

  private def runtimeError(): Nothing = throw new Error("unexpected runtime call")

}
