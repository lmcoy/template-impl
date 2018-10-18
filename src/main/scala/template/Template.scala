package template
import java.io.{Reader, StringReader}

/**
  * representation of a template containing variables of the form ${...}.
  *
  * A variable must have the form ${name} where name can contain any character
  * except '}'. If no closing '}' is found for a "${" sequence that would start
  * a variable, the whole text is read as normal text. If a '$' without a following
  * '{' is found, it is read as normal text not as start of a variable.
  * Using "$${name}" will be read as text "${name}" and not as variable. A "$$" will
  * be replace by a single '$'.
  *
  * Examples:
  *   Template("hello ${s} ll ${y}").replace(Map("s" -> "test", "y" -> "test2"))  == "hello test ll test2"
  *   Template("${x}").replace(Map())  == "${x}"
  *   Template("${x}").replace(Map("x" -> "text"))  == "text"
  *   Template("$${x}").replace(Map("x" -> "test"))  == "${x}"
  *   Template("$abb").replace(Map("x" -> "test"))  == "$abb"
  *   Template("$x").replace(Map("x" -> "test"))  == "$x"
  *   Template("hello $x ll $y").replace(Map("x" -> "test", "y" -> "test2"))  == "hello $x ll $y"
  *   Template("hello $x ll $").replace(Map("x" -> "test", "y" -> "test2"))  == "hello $x ll $"
  *   Template("test ${x").replace(Map("x" -> "test"))  == "test ${x"
  *   Template("hello $$ hello").replace(Map())  == "hello $ hello"
  *   Template("hello $$$$ hello").replace(Map())  == "hello $$ hello"
  *
  * A template is a list of tokens (variable, text). The text token
  * contains normal text and the variable token contains a variable
  * with a name. The Template can be converted into a String by
  * calling the `replace` method.
  */
trait Template {

  // replace all variables with the values defined in `vars`
  def replace(vars: Map[String, String]): String

}

object Template {
  sealed trait Implementation

  case object Replace extends Implementation
  case object Imperative extends Implementation
  case object Func1 extends Implementation
  case object IO extends Implementation
  case object IOCats extends Implementation

  def apply(reader: Reader)(implicit impl: Implementation): Template =
    impl match {
      case Replace => TemplateReplace(reader)
      case Imperative => TemplateImperative(reader)
      case Func1      => TemplateFunc1(reader)
      case IO         => TemplateIO.getIO(reader).run
      case IOCats     => TemplateIOCats.getIO(reader).unsafeRunSync()
    }

  def apply(s: String)(implicit impl: Implementation): Template =
    Template(new StringReader(s))
}


