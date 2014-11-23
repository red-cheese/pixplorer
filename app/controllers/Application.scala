package controllers

import org.jsoup.nodes.Element
import org.jsoup.select.Elements
import play.api._
import play.api.data._
import play.api.data.Forms._
import play.api.mvc._
import scala.collection.mutable._

// Scala webscraper to extract image src's from web page
import net.ruippeixotog.scalascraper.browser.Browser
import net.ruippeixotog.scalascraper.dsl.DSL._
import net.ruippeixotog.scalascraper.dsl.DSL.Extract._
import net.ruippeixotog.scalascraper.dsl.DSL.Parse._

case class URL(url: String)

object Application extends Controller {

  val urlForm = Form(
    mapping(
      "url" -> play.api.data.Forms.text
    )(URL.apply)(URL.unapply)
  )

  val browser = new Browser

  def index = Action {
    Ok(views.html.index(urlForm))
  }

  def pixplorer = Action { implicit request =>
    val urlData = urlForm.bindFromRequest.get
    val url = urlData.url


    // Pixplore!

    // @TODO: URL must be valid (http://www...)
    val doc = browser.get(url)
    val imgElements: Elements = doc >> elements("img")
    val images: java.util.Iterator[Element] = imgElements.iterator()
    var imgSrcs = new HashSet[String]
    while (images.hasNext()) {
      var src = images.next().attr("src")
      // @TODO: Handle local files: append http://domain
      val protocols = Array("http://", "https://", "ftp://")
      if (protocols.foldLeft(false) { (x, y) => x || src.startsWith(y) } ) {
	imgSrcs += src
      }
    }
    imgSrcs.foreach(println)


    // Redirect after POST
    Redirect(routes.Application.results(urlData.url))
  }

  def results(url: String) = Action {
    Ok(views.html.results(url))
  }

}
