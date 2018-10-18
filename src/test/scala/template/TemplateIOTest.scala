package template

import java.io.StringReader

import org.scalatest.{FlatSpec, Matchers}

class TemplateIOTest extends FlatSpec with Matchers {

  "Replace" should "replace all variables" in {
    TemplateIO.getIO(new StringReader("hello ${s} ll ${y}")).run.replace(Map("s" -> "test", "y" -> "test2")) should equal("hello test ll test2")
    TemplateIO.getIO(new StringReader("${x}")).run.replace(Map()) should equal ("${x}")
    TemplateIO.getIO(new StringReader("${x}")).run.replace(Map("x" -> "text")) should equal ("text")
  }

  it should "consider $$ as literal $" in {
    TemplateIO.getIO(new StringReader("$${x}")).run.replace(Map("x" -> "test")) should equal ("${x}")
    TemplateIO.getIO(new StringReader("hello $$ hello")).run.replace(Map()) should equal ("hello $ hello")
    TemplateIO.getIO(new StringReader("hello $$$$ hello")).run.replace(Map()) should equal ("hello $$ hello")
  }

  it should "read $ as text if it is not part of a variable ${...}" in {
    TemplateIO.getIO(new StringReader("$x")).run.replace(Map("x" -> "test")) should equal ("$x")
    TemplateIO.getIO(new StringReader("hello $x ll $y")).run.replace(Map("x" -> "test", "y" -> "test2")) should equal ("hello $x ll $y")
    // $ as end of input
    TemplateIO.getIO(new StringReader("hello $x ll $")).run.replace(Map("x" -> "test", "y" -> "test2")) should equal("hello $x ll $")
    // no closing }
    TemplateIO.getIO(new StringReader("test ${x")).run.replace(Map("x" -> "test")) should equal ("test ${x")
  }
}
