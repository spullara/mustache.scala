package com.sampullara.mustache.scala

import org.specs.Specification
import java.io.StringWriter
import com.sampullara.mustache.{Scope, Mustache}
import scala.Boolean

/**
 * Tests for the builder.
 */
object BuilderSpec extends Specification {
  "builder should produce match" in {
    val builder = new Builder("templates")
    val mustache: Mustache = builder.parseFile("complex.html")
    val writer = new StringWriter
    mustache.execute(writer, new Scope(new {
      val header: String = "Colors"
      val item = List(new {
        val name: String = "red"
        val current: Boolean = true
        val url: String = "#Red"
      }, new {
        val name: String = "green"
        val current: Boolean = false
        val url: String = "#Green"
      }, new {
        val name: String = "blue"
        val current: Boolean = false
        val url: String = "#Blue"
      })
      def link(s:Scope):Boolean = {
        !(s.get("current").asInstanceOf[Boolean])
      }

      def list(s:Scope):Boolean = {
        s.get("item").asInstanceOf[List[_]].size != 0;
      }

      def empty(s:Scope):Boolean = {
        s.get("item").asInstanceOf[List[_]].size == 0;
      }

    }))
    val string: String = writer.toString
    println(string)
    string must_== """<h1>Colors</h1>
  <ul>
      <li><strong>red</strong></li>
      <li><a href="#Green">green</a></li>
      <li><a href="#Blue">blue</a></li>
  </ul>
  <p>The list is not empty.</p>
"""
  }
}