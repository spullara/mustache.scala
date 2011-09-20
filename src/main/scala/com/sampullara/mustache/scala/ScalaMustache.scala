package com.sampullara.mustache.scala

import collection.JavaConversions
import com.twitter.util.Future
import com.sampullara.mustache.{Mustache, Scope}

/**
 * Converter. Will be replaced with a native Mustache implementation.
 */
class ScalaMustache extends Mustache {
  def unwrap(value: Object, wrapFuture:Boolean = true): Object = {
    value match {
      case i: Iterable[Object] => JavaConversions.asJavaIterable(i)
      case o: Option[Object] => o match {
        case Some(some: Object) => unwrap(some)
        case None => null
      }
      case f: Future[Object] => if (wrapFuture) {
        (f map {
          value => unwrap(value, false)
        }).toJavaFuture
      } else {
        unwrap(f.get(), false)
      }
      case _ => value
    }
  }

  override def getValue(s: Scope, name: String): Object = {
    val unwrapped = unwrap(super.getValue(s, name))
    println(name + ": " + unwrapped)
    unwrapped
  }
}