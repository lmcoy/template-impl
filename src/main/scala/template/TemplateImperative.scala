package template

import java.io.{PushbackReader, Reader}

import scala.collection.mutable

import TemplateImperative.Token
import TemplateImperative.Text
import TemplateImperative.Variable

class TemplateImperative(tokens: List[Token]) extends Template {

  def replace(vars: Map[String, String]): String = {
    val out = new StringBuilder("")
    for (token <- tokens) {
      val s = token match {
        case Text(text)     => text
        case Variable(name) => vars.getOrElse(name, s"$${$name}")
      }
      out.appendAll(s)
    }
    out.toString()
  }
}


object TemplateImperative {
  private  sealed trait Token

  private  case class Variable(name: String) extends Token
  private  case class Text(text: String) extends Token

  def apply(reader: Reader): TemplateImperative = new TemplateImperative(read(reader))

  private def read(reader: Reader): List[Token] = {
    val list = mutable.Buffer[Token]()
    val pbReader = new PushbackReader(reader)

    def readVariable: Option[Token] = {
      val start = reader.read()
      if (start != '{') {
        if (start != '$' && start != -1)
          pbReader.unread(start)
        None
      } else {
        val variable = new StringBuilder("")
        var next = pbReader.read()
        var eof = false
        while (next != '}' && !eof) {
          if (next == -1) {
            eof = true
          } else {
            variable.append(next.toChar)
            next = pbReader.read()
          }
        }
        if (!eof) Some(Variable(variable.toString()))
        else Some(Text(s"$${${variable.toString()}"))
      }
    }

    val text = new StringBuilder("")
    var next = pbReader.read()
    while (next != -1) {
      if (next == '$') {
        readVariable match {
          case Some(tv) =>
            tv match {
              case _: Variable =>
                if (text.nonEmpty)
                  list.append(Text(text.toString()))
                text.clear()
                list.append(tv)
              case t: Text =>
                text.append(t.text)
            }
          case None =>
            text.append(next.toChar)
        }
      } else text.append(next.toChar)
      next = pbReader.read()
    }
    if (text.nonEmpty)
      list.append(Text(text.toString()))
    list.toList
  }
}
