package com.sampullara.mustache.scala

import java.util.concurrent.atomic.AtomicInteger
import com.google.common.base.Charsets
import java.io._
import com.sampullara.mustache._
import collection.mutable._
import collection.JavaConversions.asJavaList

/**
 * Scala version of Mustache Builder. Converted directly from Java version, slowly
 * converted to more idiomatic Scala over time.
 *
 * User: sam
 * Date: 9/18/11
 * Time: 12:46 PM
 */
class Builder(classpathRoot: String) extends MustacheJava {
  private var superclass: Class[_ <: Mustache] = classOf[ScalaMustache]
  private val cf: CodeFactory = new DefaultCodeFactory

  def setSuperclass(superclass: String) {
    try {
      this.superclass = Class.forName(superclass).asInstanceOf[Class[_ <: Mustache]]
    }
    catch {
      case e: Exception => {
        throw new IllegalArgumentException("Invalid class", e)
      }
    }
  }

  def parse(template: String): Mustache = {
    build(new StringReader(template), template)
  }

  def build(br: Reader, file: String): Mustache = {
    var mustache: Mustache = null
    try {
      mustache = if (superclass == null) new Mustache else superclass.newInstance
    }
    catch {
      case e: Exception => {
        throw new IllegalArgumentException("Could not instantiate", e)
      }
    }
    mustache.setPath(file)
    mustache.setMustacheJava(this)
    mustache.setCompiled(compile(mustache, br, null, new AtomicInteger(0), file))
    mustache
  }

  def parseFile(path: String): Mustache = {
    var compile: Mustache = null
    try {
      var br: BufferedReader = null
      val fullPath: String = if (classpathRoot == null) path else classpathRoot + "/" + path
      val resourceAsStream: InputStream = classOf[MustacheBuilder].getClassLoader.getResourceAsStream(
        fullPath)
      if (resourceAsStream == null) {
        throw new MustacheException(path + " not found in classpath")
      }
      br = new BufferedReader(new InputStreamReader(resourceAsStream, Charsets.UTF_8))
      compile = build(br, path)
      br.close()
    }
    catch {
      case e: IOException => {
        throw new MustacheException("Failed to read " + new File(classpathRoot, path), e)
      }
    }
    compile
  }

  protected def compile(m: Mustache, br: Reader, tag: String, currentLine: AtomicInteger, file: String): Buffer[Code] = {
    val list: Buffer[Code] = Buffer()
    val sm: String = "{{"
    val em: String = "}}"
    var c: Int = 0
    var onlywhitespace: Boolean = true
    var iterable: Boolean = currentLine.get != 0
    currentLine.compareAndSet(0, 1)
    var out: StringBuilder = new StringBuilder
    try {
      while ((({
        c = br.read;
        c
      })) != -1) {
        if (c == '\r') {
        } else if (c == '\n') {
          currentLine.incrementAndGet
          if (!iterable || (iterable && !onlywhitespace)) {
            out.append("\n")
          }
          out = write(list, out, currentLine.intValue)
          iterable = false
          onlywhitespace = true
        } else if (c == sm.charAt(0)) {
          br.mark(1)
          if (br.read == sm.charAt(1)) {
            val sb: StringBuilder = new StringBuilder
            var append = true
            while (append && (({
              c = br.read;
              c
            })) != -1) {
              br.mark(1)
              if (c == em.charAt(0)) {
                if (br.read == em.charAt(1)) {
                  append = false
                }
                else {
                  br.reset()
                }
              }
              if (append) {
                sb.append(c.asInstanceOf[Char])
              }
            }
            val command: String = sb.toString().trim
            val ch: Char = command.charAt(0)
            val variable: String = command.substring(1).trim
            ch match {
              case '#' | '^' | '_' | '?' => {
                val start: Int = currentLine.get
                val codes: Buffer[Code] = compile(m, br, variable, currentLine, file)
                val lines: Int = currentLine.get - start
                if (!onlywhitespace || lines == 0) {
                  write(list, out, currentLine.intValue)
                }
                out = new StringBuilder
                ch match {
                  case '#' =>
                    list += cf.iterable(m, variable, codes, file, currentLine.get)
                  case '^' =>
                    list += cf.notIterable(m, variable, codes, file, currentLine.get)
                  case '?' =>
                    list += cf.ifIterable(m, variable, codes, file, currentLine.get)
                  case '_' =>
                    list += cf.function(m, variable, codes, file, currentLine.get)
                }
                iterable = lines != 0
              }
              case '/' => {
                if (!onlywhitespace) {
                  write(list, out, currentLine.intValue)
                }
                if (!(variable == tag)) {
                  throw new MustacheException(
                    "Mismatched start/end tags: " + tag + " != " + variable + " in " + file + ":" + currentLine)
                }
                return list
              }
              case '>' => {
                out = write(list, out, currentLine.intValue)
                list += cf.partial(m, variable, file, currentLine.get)
              }
              case '{' => {
                out = write(list, out, currentLine.intValue)
                var name: String = variable
                if (em.charAt(1) != '}') {
                  name = variable.substring(0, variable.length - 1)
                }
                else {
                  if (br.read != '}') {
                    throw new MustacheException(
                      "Improperly closed variable in " + file + ":" + currentLine)
                  }
                }
                val finalName: String = name
                list += cf.value(m, finalName, false, currentLine.intValue)
              }
              case '&' => {
                out = write(list, out, currentLine.intValue)
                list += cf.value(m, variable, false, currentLine.intValue)
              }
              case '%' =>
                out = write(list, out, currentLine.intValue)
              case '!' =>
                out = write(list, out, currentLine.intValue)
              case _ => {
                out = write(list, out, currentLine.intValue)
                list += cf.value(m, command, true, currentLine.intValue)
              }
            }
          } else {
            br.reset()
          }
        } else {
          onlywhitespace = (c == ' ' || c == '\t') && onlywhitespace
          out.append(c.asInstanceOf[Char])
        }
      }
      write(list, out, currentLine.intValue)
    } catch {
      case e: IOException => {
        throw new MustacheException("Failed to read", e)
      }
    }
    list += cf.eof(currentLine.intValue)
    list
  }

  /**
   * Ignore empty strings and append to the previous code if it was also a write.
   */
  private def write(list: Buffer[Code], out: StringBuilder, line: Int): StringBuilder = {
    val text: String = out.toString()
    if (text.length > 0) {
      val size: Int = list.size
      var code: Code = null
      if (size > 0 && (({
        code = list.get(size - 1);
        code
      })).isInstanceOf[WriteCode]) {
        val writeCode: WriteCode = code.asInstanceOf[WriteCode]
        writeCode.append(text)
      }
      else {
        list += cf.write(text, line)
      }
    }
    new StringBuilder
  }
}