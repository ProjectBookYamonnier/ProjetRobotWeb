package application
import library.Html
import library.OutilsWebObjet
import library.Tag
import library.Expression
import library.Texte
import application.FiltrageURLs
import application.FiltrageHtml
import library.AnalysePage

class AnalysePageClass extends AnalysePage {
  val objFiltrageUrls: FiltrageURLs = new FiltrageURLs()
  val objFiltrageHtml: FiltrageHtml = new FiltrageHtml()

  /** A partir d'une URL de requête sur le site de référence et d'une expression
    * exp, retourne de pages issues de la requête et satisfaisant l'expression.
    *
    * @param url
    *   l'URL de la requête sur le site de référence
    * @param exp
    *   l'expression à vérifier sur les pages trouvées
    * @return
    *   la liste des couples (titre,ref) où ref est l'URL d'une page
    *   satisfaisant l'expression et titre est son titre.
    */
  def resultats(url: String, exp: Expression): List[(String, String)] = {

    var listeAnnonces: List[(String, Html)] = List()

    for (s <- objFiltrageUrls.filtreAnnonce(OutilsWebObjet.obtenirHtml(url))) {
      listeAnnonces = (s, OutilsWebObjet.obtenirHtml(s)) :: listeAnnonces
    }

    var listeAnnoncesReq: List[(String, Html)] = List()

    for (c <- listeAnnonces) {
      c match {
        case (_, h) =>
          if (objFiltrageHtml.filtreHtml(h, exp)) {

            listeAnnoncesReq = c :: listeAnnoncesReq
          }
      }
    }

    var res: List[(String, String)] = List()
    for (c <- listeAnnoncesReq) {
      c match {
        case (lien, page) => res = res :+ (extraitTitre(page), lien)
      }
    }

    res

  }

  def extraitTitre(h: Html): String = {
    h match {
      case Tag(name, attributes, children) =>
        if (name == "title") {
          children match {
            case List(Texte(titredelapage)) => titredelapage
            case _                          => ""
          }
        } else {
          var r: String = ""
          for (c <- children) {
            r += extraitTitre(c)
          }
          r
        }
      case Texte(_) => ""

    }
  }

}
