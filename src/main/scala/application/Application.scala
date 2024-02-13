package application
import java.io.FileWriter
import library.Expression
import library.Mot
import library.Et
import library.Ou
import library.OutilsWebObjet
import library.ParserExpression

object Application extends App {

  val prod: ProductionResultatClass = new ProductionResultatClass()
  val htmlvstring: HtmlVersStringClass = new HtmlVersStringClass()
  val analysePage: AnalysePageClass = new AnalysePageClass()
  val siteRef: String =
    "https://search.vivastreet.com/annonces/fr?start_field=1&keywords="

  var expr: Expression = ParserExpression.lireExpression

  def distribue(e: Expression): Expression = {
    e match {
      case Et(Ou(e1, e2), Ou(e3, e4)) =>
        Ou(
          Ou(
            Ou(
              Et(distribue(e2), distribue(e4)),
              Et(distribue(e1), distribue(e4))
            ),
            Et(distribue(e2), distribue(e3))
          ),
          Et(distribue(e1), distribue(e3))
        )
      case Et(Ou(e1, e2), e3) =>
        Ou(Et(distribue(e1), distribue(e3)), Et(distribue(e2), distribue(e3)))
      case Et(e1, Ou(e2, e3)) =>
        Ou(Et(distribue(e1), distribue(e2)), Et(distribue(e1), distribue(e3)))
      case Et(e1, e2) => Et(distribue(e1), distribue(e2))
      case Ou(e1, e2) => Ou(distribue(e1), distribue(e2))
      case Mot(w)     => Mot(w)

    }
  }

  def analysePageFinal(e: Expression): List[(String, String)] = {
    e match {
      case Mot(a) =>
        analysePage.resultats(
          siteRef ++ a,
          e
        )
      case Et(e1, e2) =>
        analysePage.resultats(
          siteRef ++ (exprToString(e1) + "+" + exprToString(e2)),
          e
        )
      case Ou(e1, e2) =>
        analysePageFinal(e1) ++ analysePageFinal(e2)
    }
  }

  def exprToString(e: Expression): String = {
    e match {
      case Mot(a) => a
      case Et(e1, e2) =>
        exprToString(e1) + "+" + exprToString(e2)
      case Ou(e1, e2) =>
        exprToString(e1) + "+" + exprToString(e2)

    }
  }

  var res: String = htmlvstring.traduire(
    prod.resultatVersHtml(
      analysePageFinal(distribue(expr))
    )
  )

  val file = new FileWriter("monFichier.html")
  try {
    file.write(res)
  } finally file.close()

}
