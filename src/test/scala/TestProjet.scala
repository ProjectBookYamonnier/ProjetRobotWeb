import org.junit.Assert._
import org.junit.Test
import library.HtmlExample
import library.Expression
import library.Mot
import library.Et
import library.Ou
import application.FiltrageURLs
import application.AnalysePageClass
import application.FiltrageHtml
import application.ProductionResultatClass
import java.io.FileWriter
import library.Expression
import library.Html
import library.Tag
import library.Texte

class TestProjet {

  @Test
  def testFiltrageHtml(): Unit = {
    val filtrageHtml: FiltrageHtml = new FiltrageHtml()
    val motUn: Expression = Mot("Lien")
    val motDeux: Expression = Mot("ll")
    val motTrois: Expression = Mot("Mr")
    val exp: Expression = Et(motUn, Ou(motDeux, motTrois))
    val exp2: Expression = Ou(motUn, exp)
    assertFalse(
      "TestMotInconnu",
      filtrageHtml.filtreHtml(HtmlExample.exemple, motDeux)
    )
    assertTrue(
      "TestMotConnu",
      filtrageHtml.filtreHtml(HtmlExample.exemple, motUn)
    )
    assertFalse(
      "TestExpOuFalse",
      filtrageHtml.filtreHtml(HtmlExample.exemple, Ou(motDeux, motTrois))
    )
    assertFalse(
      "TestExpOuFalseInv",
      filtrageHtml.filtreHtml(HtmlExample.exemple, Ou(motTrois, motDeux))
    )
    assertTrue(
      "TestExpOuTrue",
      filtrageHtml.filtreHtml(HtmlExample.exemple, Ou(motUn, motDeux))
    )
    assertTrue(
      "TestExpOuTrueInv",
      filtrageHtml.filtreHtml(HtmlExample.exemple, Ou(motDeux, motUn))
    )
    assertTrue(
      "TestExpOuTrue2",
      filtrageHtml.filtreHtml(HtmlExample.exemple, Ou(motUn, motUn))
    )
    assertTrue(
      "TestEtTrue",
      filtrageHtml.filtreHtml(HtmlExample.exemple, Et(motUn, motUn))
    )
    assertFalse(
      "TestEtFalse",
      filtrageHtml.filtreHtml(HtmlExample.exemple, Et(motUn, motDeux))
    )
    assertFalse(
      "TestEtFalse",
      filtrageHtml.filtreHtml(HtmlExample.exemple, Et(motTrois, motDeux))
    )
    assertTrue(
      "TestExpTrue",
      filtrageHtml.filtreHtml(
        HtmlExample.exemple,
        Ou(motUn, Et(motUn, Ou(motDeux, motTrois)))
      )
    )

  }

  @Test
  def testFiltrageUrls(): Unit = {
    val filtreAnnonce: FiltrageURLs = new FiltrageURLs()
    println(filtreAnnonce.filtreAnnonce(HtmlExample.exemple))
    assertTrue(
      "TestAnnonce1",
      filtreAnnonce.filtreAnnonce(HtmlExample.exemple) == List(
        "http://www.irisa.fr/123456"
      )
    )
    assertFalse(
      "TestAnnonce2",
      filtreAnnonce.filtreAnnonce(HtmlExample.exemple) == List()
    )
    assertFalse(
      "TestAnnonce3",
      filtreAnnonce.filtreAnnonce(HtmlExample.exemple) == List(
        "http://www.irisa.fr/12345c6"
      )
    )
  }

}
