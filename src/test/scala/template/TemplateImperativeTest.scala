package template

import java.io.StringReader

import org.scalatest.{FlatSpec, Matchers}

class TemplateImperativeTest extends FlatSpec with Matchers {

  "Replace" should "replace all variables" in {
    TemplateImperative(new StringReader("hello ${s} ll ${y}")).replace(Map("s" -> "test", "y" -> "test2")) should equal("hello test ll test2")
    TemplateImperative(new StringReader("${x}")).replace(Map()) should equal ("${x}")
    TemplateImperative(new StringReader("${x}")).replace(Map("x" -> "text")) should equal ("text")
  }

  it should "consider $$ as literal $" in {
    TemplateImperative(new StringReader("$${x}")).replace(Map("x" -> "test")) should equal ("${x}")
    TemplateImperative(new StringReader("hello $$ hello")).replace(Map()) should equal ("hello $ hello")
    TemplateImperative(new StringReader("hello $$$$ hello")).replace(Map()) should equal ("hello $$ hello")
  }

  it should "read $ as text if it is not part of a variable ${...}" in {
    TemplateImperative(new StringReader("$x")).replace(Map("x" -> "test")) should equal ("$x")
    TemplateImperative(new StringReader("hello $x ll $y")).replace(Map("x" -> "test", "y" -> "test2")) should equal ("hello $x ll $y")
    // $ as end of input
    TemplateImperative(new StringReader("hello $x ll $")).replace(Map("x" -> "test", "y" -> "test2")) should equal("hello $x ll $")
    // no closing }
    TemplateImperative(new StringReader("test ${x")).replace(Map("x" -> "test")) should equal ("test ${x")
  }
}
