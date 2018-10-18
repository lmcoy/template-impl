package template
import java.io.StringReader

import org.scalatest.{FlatSpec, Matchers}

class TemplateFunc1Test extends FlatSpec with Matchers {

  "Replace" should "replace all variables" in {
    TemplateFunc1(new StringReader("hello ${s} ll ${y}")).replace(Map("s" -> "test", "y" -> "test2")) should equal("hello test ll test2")
    TemplateFunc1(new StringReader("${x}")).replace(Map()) should equal ("${x}")
    TemplateFunc1(new StringReader("${x}")).replace(Map("x" -> "text")) should equal ("text")
  }

  it should "consider $$ as literal $" in {
    TemplateFunc1(new StringReader("$${x}")).replace(Map("x" -> "test")) should equal ("${x}")
    TemplateFunc1(new StringReader("hello $$ hello")).replace(Map()) should equal ("hello $ hello")
    TemplateFunc1(new StringReader("hello $$$$ hello")).replace(Map()) should equal ("hello $$ hello")
  }

  it should "read $ as text if it is not part of a variable ${...}" in {
    TemplateFunc1(new StringReader("$x")).replace(Map("x" -> "test")) should equal ("$x")
    TemplateFunc1(new StringReader("hello $x ll $y")).replace(Map("x" -> "test", "y" -> "test2")) should equal ("hello $x ll $y")
    // $ as end of input
    TemplateFunc1(new StringReader("hello $x ll $")).replace(Map("x" -> "test", "y" -> "test2")) should equal("hello $x ll $")
    // no closing }
    TemplateFunc1(new StringReader("test ${x")).replace(Map("x" -> "test")) should equal ("test ${x")
  }
}
