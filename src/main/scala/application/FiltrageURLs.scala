package application

import scala.annotation.switch
import javax.swing.text.html.HTML
import java.net.URL
import library.Html
import library.Tag
import library.Texte

class FiltrageURLs extends library.FiltrageURLs {

  def filtreAnnonce(h: Html): List[String] = {
    filtreURL(filtreAnnonceBis(h)).distinct
  }

  private def filtreAnnonceBis(h: Html): List[String] = h match {
    case Texte(_) => Nil
    case Tag("a", attributes, children) =>
      val hrefAttrs = attributes.filter(attr => attr._1 == "href")
      hrefAttrs.map(_._2) ++ children.flatMap(filtreAnnonceBis)
    case Tag(_, _, children) => children.flatMap(filtreAnnonceBis)
  }

  /** Crée une liste d'URL provenant d'annonces
    *
    * @param l
    *   une liste d'URL présente sur le site
    * @return
    */
  def filtreURL(l: List[String]): List[String] = {
    var lV = List[String]()
    for (id <- l) {
      val tab = id.split("/")
      if (estUnEntier(tab(tab.length - 1))) {
        lV = lV :+ id
      }
    }
    lV
  }

  /** Renvoie un boolean True ssi str est un entier
    *
    * @param str
    *   un String
    * @return
    *   un Boolean
    */
  def estUnEntier(str: String): Boolean = {
    (toInt(str)) match {
      case None => false
      case _    => true
    }
  }

  /** Renvoie un entier ssi il n'y a pas d'exception sinon renvoie None
    *
    * @param s
    *   Un String
    * @return
    *   un Integer ou None
    */
  private def toInt(s: String): Option[Integer] = {
    try {
      Some(s.toInt)
    } catch {
      case e: NumberFormatException => None
    }
  }

}
