package application
import library.Expression
import library.Html
import library.Tag
import library.Texte
import library.Mot
import library.Et
import library.Ou

class FiltrageHtml extends library.FiltrageHtml {

  def filtreHtml(h: Html, e: Expression): Boolean = {
    h match {
      case Texte(content) =>
        if (contenir(e, content)) return true
        else false
      case Tag(name, attributes, children) =>
        childrenIterator(children, e)
    }
  }

  def childrenIterator(children: List[Html], e: Expression): Boolean = {
    children match {
      case Nil          => false
      case head :: next => filtreHtml(head, e) || childrenIterator(next, e)

    }
  }

  /** a partir d'une expression et d'une chaine de character , verifier si la
    * chaine inclut l'expression
    *
    * @param exp
    *   une expression à chercher
    * @param str
    *   un String
    * @return
    *   true si le document satisfait l'expression
    */

  /** a partir d'une expression et d'une chaine de character , verifier si la
    * chaine inclut l'expression
    *
    * @param exp
    *   une expression à chercher
    * @param str
    *   un String
    * @return
    *   true si le document satisfait l'expression
    */
  def contenir(exp: Expression, str: String): Boolean = {
    exp match {
      case Mot(a) => if (str.contains(a)) true else false
      case Et(e1, e2) =>
        if (contenir(e1, str) && contenir(e2, str)) true else false
      case Ou(e1, e2) =>
        if (contenir(e1, str) || contenir(e2, str)) true else false

    }
  }

}
