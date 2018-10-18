import template.Template

object TemplateTest extends App {
  implicit val implementation = Template.Replace

  def test(str: String, expected: String) = {
    print(str)
    if (str == expected) println("   PASSED")
    else println(s"    FAILED expected $expected")
  }

  test(
    Template("hello ${s} ll ${y}").replace(Map("s" -> "test", "y" -> "test2")),
    "hello test ll test2"
  )
  test(Template("${x}").replace(Map()), "${x}")
  test(Template("${x}").replace(Map("x" -> "text")), "text")
  test(Template("$${x}").replace(Map("x" -> "test")), "${x}")
  test(Template("$abb").replace(Map("x" -> "test")), "$abb")
  test(Template("$x").replace(Map("x" -> "test")), "$x")
  test(
    Template("hello $x ll $y").replace(Map("x" -> "test", "y" -> "test2")),
    "hello $x ll $y"
  )
  test(
    Template("hello $x ll $").replace(Map("x" -> "test", "y" -> "test2")),
    "hello $x ll $"
  )
  test(Template("test ${x").replace(Map("x" -> "test")), "test ${x")
  test(Template("hello $$ hello").replace(Map()), "hello $ hello")
  test(Template("hello $$$$ hello").replace(Map()), "hello $$ hello")
}
