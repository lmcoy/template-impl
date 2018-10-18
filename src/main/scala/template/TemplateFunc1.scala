package template
import java.io.Reader

import TemplateFunc1.Token
import TemplateFunc1.Text
import TemplateFunc1.Variable

import scala.annotation.tailrec

class TemplateFunc1(tokens: List[Token]) extends Template {
  // replace all variables with the values defined in `vars`
  def replace(vars: Map[String, String]): String = {
    @tailrec
    def go(acc: StringBuilder, list: List[Token]): StringBuilder = {
      list match {
        case Nil => acc
        case token :: toks =>
          val s = token match {
            case Text(text) => acc.appendAll(text)
            case Variable(name) =>
              acc.appendAll(vars.getOrElse(name, s"$${$name}"))
          }
          go(s, toks)
      }
    }

    go(new StringBuilder(""), tokens.reverse).toString()
  }

}

object TemplateFunc1 {
  private  sealed trait Token

  private  case class Variable(name: String) extends Token
  private  case class Text(text: String) extends Token

  def apply(reader: Reader): TemplateFunc1 = new TemplateFunc1(read(reader))

  private def read(reader: Reader): List[Token] = {

    type Character = Int
    val EOF = -1

    case class ContentAndUnusedChar[T](unusedChar: Option[Character],
                                       content: T) {
      def map[A](f: T => A): ContentAndUnusedChar[A] =
        ContentAndUnusedChar(unusedChar, f(content))
    }

    object ContentAndUnusedChar {
      def apply[T](unusedChar: Character,
                   content: T): ContentAndUnusedChar[T] =
        new ContentAndUnusedChar(Some(unusedChar), content)
      def apply[T](content: T): ContentAndUnusedChar[T] =
        new ContentAndUnusedChar(None, content)
    }

    // read a variable of the form {...}.
    def readVariable: ContentAndUnusedChar[Option[Token]] = {
      reader.read() match {
        case EOF => ContentAndUnusedChar(None)
        case '{' =>
          @tailrec
          def go(builder: StringBuilder): Token = {
            reader.read() match {
              case '}' => Variable(builder.toString())
              case EOF => Text(s"$${${builder.toString()}")
              case c   => go(builder.append(c.toChar))
            }
          }
          ContentAndUnusedChar(Some(go(new StringBuilder())))
        case '$' => ContentAndUnusedChar(None)
        case c   => ContentAndUnusedChar(c, None)
      }
    }

    def readText(t: StringBuilder): ContentAndUnusedChar[Text] = {
      @tailrec
      def go(text: StringBuilder): ContentAndUnusedChar[StringBuilder] = {
        reader.read() match {
          case '$' => ContentAndUnusedChar('$', text)
          case EOF => ContentAndUnusedChar(text)
          case c   => go(text.append(c.toChar))
        }
      }
      go(t).map(content => Text(content.toString()))
    }

    @tailrec
    def go(acc: ContentAndUnusedChar[List[Token]]): List[Token] = {
      def prependTextToAcc(text: Text) =
        acc.content match {
          case Nil => List(text)
          case x :: xs =>
            x match {
              case Text(t) => Text(t + text.text) :: xs
              case _       => text :: x :: xs
            }
        }
      acc.unusedChar.getOrElse(reader.read()) match {
        case EOF => acc.content
        case '$' =>
          val maybeVariable = readVariable
          maybeVariable.content match {
            case Some(v: Variable) =>
              go(ContentAndUnusedChar(v :: acc.content))
            case Some(v: Text) =>
              go(readText(new StringBuilder(v.text)).map(prependTextToAcc))
            case None =>
              val char =
                maybeVariable.unusedChar.fold("$")(c => s"$$${c.toChar}")
              go(readText(new StringBuilder(char)).map(prependTextToAcc))
          }
        case c =>
          go(
            readText(new StringBuilder(s"${c.toChar}")).map(prependTextToAcc)
          )
      }
    }
    go(ContentAndUnusedChar(Nil))
  }

}
