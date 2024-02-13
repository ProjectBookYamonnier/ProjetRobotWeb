package application
import library.Html
import library.Tag
import library.Texte

class HtmlVersStringClass extends library.HtmlVersString {

  /** Produit la chaîne de caractère correspondant à un document Html
    *
    * @param h
    *   le document Html
    * @return
    *   la chaîne de caractère représentant h
    */
  def traduire(h: Html): String = {
    h match {
      case Tag(name, attributes, children) =>
        "<" + name + attributesToString(
          attributes
        ) + "> \n " + childrenToString(children) + "</" + name + ">"
      case Texte(content) => content
    }
  }

  def attributesToString(attributes: List[(String, String)]): String = {
    if (attributes.isEmpty) {
      ""
    } else {
      var res: String = " "
      for (e <- attributes) {
        e match {
          case (name, value) => res += (name + "=" + '"' + value + '"' + " ")
        }
      }
      res
    }

  }

  def childrenToString(children: List[Html]): String = {
    var res: String = ""
    for (e <- children) {
      res += traduire(e) + " \n"
    }
    res
  }
}
