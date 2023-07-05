package com.github.automap.api

trait Mapper[-From, +To] {

  def map(from: From): To

  def andThen[ComposedTo](
      mapper: Mapper[To, ComposedTo]
  ): Mapper[From, ComposedTo] = (from: From) => mapper.map(map(from))

}

object Mapper {

  def identity[T]: Mapper[T, T] = (from: T) => from

}
