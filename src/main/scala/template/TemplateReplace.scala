package template
import java.io.Reader

class TemplateReplace(text: String) extends Template {
  def replace(vars: Map[String, String]): String =
    //vars.foldLeft(text)((acc,mapping) => acc.replaceAllLiterally(s"$${${mapping._1}}", mapping._2))
    vars
      .foldLeft(text) { (acc, mapping) =>
        acc.replaceAll(s"(^|[^$$])\\$$\\{${mapping._1}\\}", s"$$1${mapping._2}")
      }
      .replaceAllLiterally("$$", "$")
}

object TemplateReplace {
  def apply(reader: Reader): TemplateReplace = {
    val builder = new StringBuilder("")

    var next = reader.read()
    while (next != -1) {
      builder.append(next.toChar)
      next = reader.read()
    }
    new TemplateReplace(builder.toString())
  }
}
