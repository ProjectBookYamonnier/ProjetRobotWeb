package application
import library.Html
import library.Tag
import library.Texte

class ProductionResultatClass extends library.ProductionResultat {

  /** A partir d’une liste de couples (titre,URL), produit un document Html, qui
    * liste les solutions sous la forme de liens cliquables
    * @param l
    *   la liste des couples solution (titre,URL)
    * @return
    *   le document Html listant les solutions
    */
  def resultatVersHtml(l: List[(String, String)]): Html = {
    Tag(
      "html",
      List(),
      List(
        Tag(
          "head",
          List(),
          List(
            Tag("meta", List(("charset", "utf-8")), List()),
            Tag("title", List(), List(Texte("Résultats de recherche"))),
            Tag(
              "link",
              List(("rel", "stylesheet"), ("href", "style.css")),
              List()
            )
          )
        ),
        Tag(
          "body",
          List(),
          List(
            Tag(
              "header",
              List(),
              List(
                Tag(
                  "img",
                  List(
                    ("src", "chatgpt.png")
                  ),
                  List()
                ),
                Tag(
                  "h1",
                  List(),
                  List(Texte("Giga Tchad GPT"))
                )
              )
            ),
            Tag(
              "div",
              List(("class", "ensemble")),
              List(Tag("p", List(), listeVersTag(l)))
            )
          )
        )
      )
    )

  }

  def listeVersTag(l: List[(String, String)]): List[Html] = {
    var res: List[Html] = List()

    for (e <- l) {
      e match {
        case (titre, url) =>
          res = res :+ Tag(
            "div",
            List(("class", "element")),
            List(
              Tag(
                "a",
                List(
                  ("href", url),
                  ("target", "_blank")
                ),
                List(Texte(titre))
              )
            )
          ) :+ Tag(
            "br",
            List(),
            List()
          )
      }

    }

    res

  }

}
