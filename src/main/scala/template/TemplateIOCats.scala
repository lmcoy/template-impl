package template

import java.io.Reader

import cats.effect.IO

import template.TemplateIOCats.{Text, Token, Variable}

import scala.annotation.tailrec

class TemplateIOCats(tokens: List[Token]) extends Template {
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

object TemplateIOCats {

  private sealed trait Token

  private case class Variable(name: String) extends Token
  private case class Text(text: String) extends Token

  def Read(reader: Reader) = IO[Int] { reader.read() }

  def getIO(reader: Reader): IO[TemplateIOCats] =
    for {
      tokens <- read(reader)
    } yield new TemplateIOCats(tokens)

  private def read(reader: Reader): IO[List[Token]] = {

    type Character = Int
    val EOF = -1

    case class ContentAndUnusedChar[T](unusedChar: Option[Character],
                                       content: T) {
      def map[A](f: T => A): ContentAndUnusedChar[A] =
        ContentAndUnusedChar(unusedChar, f(content))
    }

    object ContentAndUnusedChar {
      def apply[T](unusedChar: Character, content: T): ContentAndUnusedChar[T] =
        new ContentAndUnusedChar(Some(unusedChar), content)
      def apply[T](content: T): ContentAndUnusedChar[T] =
        new ContentAndUnusedChar(None, content)
    }

    // read a variable of the form {...}.
    def readVariable: IO[ContentAndUnusedChar[Option[Token]]] =
      for {
        read <- Read(reader)
        token <- read match {
          case EOF => IO { ContentAndUnusedChar[Option[Token]](None) }
          case '{' =>
            def go(builder: StringBuilder): IO[Token] =
              for {
                char <- Read(reader)
                str <- char match {
                  case '}' => IO { Variable(builder.toString()) }
                  case EOF => IO { Text(s"$${${builder.toString()}") }
                  case c   => go(builder.append(c.toChar))
                }
              } yield str
            go(new StringBuilder())
              .map(a => ContentAndUnusedChar[Option[Token]](Some(a)))
          case '$' => IO { ContentAndUnusedChar[Option[Token]](None) }
          case c   => IO { ContentAndUnusedChar[Option[Token]](c, None) }
        }
      } yield token

    def readText(t: StringBuilder): IO[ContentAndUnusedChar[Text]] = {
      def go(text: StringBuilder): IO[ContentAndUnusedChar[StringBuilder]] = {
        for {
          char <- Read(reader)
          str <- char match {
            case '$' => IO { ContentAndUnusedChar('$', text) }
            case EOF => IO { ContentAndUnusedChar(text) }
            case _   => go(text.append(char.toChar))
          }
        } yield str
      }
      go(t).map(c => c.map(content => Text(content.toString())))
    }

    def go(acc: ContentAndUnusedChar[List[Token]]): IO[List[Token]] = {
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
        case EOF => IO { acc.content }
        case '$' =>
          readVariable.flatMap { maybeVariable =>
            maybeVariable.content match {
              case Some(v: Variable) =>
                go(ContentAndUnusedChar(v :: acc.content))
              case Some(v: Text) =>
                readText(new StringBuilder(v.text))
                  .map(_.map(prependTextToAcc))
                  .flatMap(go)
              case None =>
                val char =
                  maybeVariable.unusedChar.fold("$")(c => s"$$${c.toChar}")
                readText(new StringBuilder(char))
                  .map(_.map(prependTextToAcc))
                  .flatMap(go)
            }
          }
        case c =>
          readText(new StringBuilder(s"${c.toChar}"))
            .map(_.map(prependTextToAcc))
            .flatMap(go)
      }
    }
    go(ContentAndUnusedChar(Nil))
  }
}
