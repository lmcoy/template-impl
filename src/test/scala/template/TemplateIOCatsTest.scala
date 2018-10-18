package template

import java.io.StringReader

import org.scalatest.{FlatSpec, Matchers}

class TemplateIOCatsTest extends FlatSpec with Matchers {

  "Replace" should "replace all variables" in {
    TemplateIOCats.getIO(new StringReader("hello ${s} ll ${y}")).unsafeRunSync.replace(Map("s" -> "test", "y" -> "test2")) should equal("hello test ll test2")
    TemplateIOCats.getIO(new StringReader("${x}")).unsafeRunSync.replace(Map()) should equal ("${x}")
    TemplateIOCats.getIO(new StringReader("${x}")).unsafeRunSync.replace(Map("x" -> "text")) should equal ("text")
  }

  it should "consider $$ as literal $" in {
    TemplateIOCats.getIO(new StringReader("$${x}")).unsafeRunSync.replace(Map("x" -> "test")) should equal ("${x}")
    TemplateIOCats.getIO(new StringReader("hello $$ hello")).unsafeRunSync.replace(Map()) should equal ("hello $ hello")
    TemplateIOCats.getIO(new StringReader("hello $$$$ hello")).unsafeRunSync.replace(Map()) should equal ("hello $$ hello")
  }

  it should "read $ as text if it is not part of a variable ${...}" in {
    TemplateIOCats.getIO(new StringReader("$x")).unsafeRunSync.replace(Map("x" -> "test")) should equal ("$x")
    TemplateIOCats.getIO(new StringReader("hello $x ll $y")).unsafeRunSync.replace(Map("x" -> "test", "y" -> "test2")) should equal ("hello $x ll $y")
    // $ as end of input
    TemplateIOCats.getIO(new StringReader("hello $x ll $")).unsafeRunSync.replace(Map("x" -> "test", "y" -> "test2")) should equal("hello $x ll $")
    // no closing }
    TemplateIOCats.getIO(new StringReader("test ${x")).unsafeRunSync.replace(Map("x" -> "test")) should equal ("test ${x")
  }
}
