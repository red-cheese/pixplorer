package controllers

import java.util.concurrent.Executors

/* Scala webscraper to extract image src's from web page */
import net.ruippeixotog.scalascraper.browser.Browser
import net.ruippeixotog.scalascraper.dsl.DSL._
import net.ruippeixotog.scalascraper.dsl.DSL.Extract._
import net.ruippeixotog.scalascraper.dsl.DSL.Parse._

import org.jsoup.nodes.Document
import org.jsoup.nodes.Element
import org.jsoup.select.Elements
import org.jsoup.Jsoup
import play.api._
import play.api.data._
import play.api.data.Forms._
import play.api.mvc._
import scala.collection.concurrent._
import scala.collection.mutable._
import scala.concurrent.{Future, ExecutionContext}
/* import scala.util.{Success, Failure} */

case class URL(url: String)

case class Image(src: String, pages: List[String])

object Application extends Controller {

  implicit val ec = ExecutionContext.fromExecutor(Executors.newFixedThreadPool(30))

  val urlForm = Form(
    mapping(
      "url" -> play.api.data.Forms.text
    )(URL.apply)(URL.unapply)
  )

  /* Helper vals to pixplore */
  val browser = new Browser
  val protocols = Array("http://", "https://", "ftp://")

  def index = Action {
    Ok(views.html.index(urlForm))
  }

  def reverseSearchGoogle(imgUrl: String) : Future[List[String]] = {
    /* Ask Google reverse image search and return future of results */
    val uAgent: String = "Mozilla/5.0 (X11; Linux i686) AppleWebKit/537.36 (KHTML, like Gecko) Ubuntu Chromium/37.0.2062.120 Chrome/37.0.2062.120 Safari/537.36"
    val gUrl = "http://www.google.com/searchbyimage?image_url=" + imgUrl
    val resFuture: Future[List[String]] = Future {
      val result: ListBuffer[String] = new ListBuffer[String]
      val doc: Document = Jsoup.connect(gUrl).userAgent(uAgent).timeout(60 * 1000).get
      val matches: java.util.Iterator[Element] = doc.getAllElements.select("h3.r a").iterator()
      while (matches.hasNext()) {
	val href = matches.next().attr("href")
	result += href
      }
      result.toList
    }
    /*
    /* This is for debugging purposes only */
    resFuture onComplete {
      case Success(content) => {
	println("Future resolved")
      }
      case Failure(t) => {
	println("Future rejected. Error: " + t.getMessage)
      }
    }
    */
    return resFuture
  }

  def pixplorer = Action.async { implicit request =>
    val urlData = urlForm.bindFromRequest.get
    val url = urlData.url

    /* Pixplore! */

    val doc = browser.get(url) // @TODO: URL must be valid (http://www...)
    val imgElements: Elements = doc >> elements("img")
    val images: java.util.Iterator[Element] = imgElements.iterator()
    val imgSrcs = new HashSet[String] // @TODO: inherit from SortedSet
    while (images.hasNext()) {
      val src = images.next().attr("src")
      // @TODO: Handle local files: append http://domain
      if (protocols.foldLeft(false) { (x, y) => x || src.startsWith(y) } ) {
	imgSrcs += src
      }
    }

    val imgSrcList: List[String] = imgSrcs.toList

    /* Multitasking with implicit thread pool */
    val resultFutures: List[Future[List[String]]] = imgSrcList.map {
      imgSrc => reverseSearchGoogle(imgSrc)
    }
    val futureResult: Future[List[List[String]]] = Future.sequence(resultFutures)
    futureResult.map(res => Ok(views.html.results(url, (imgSrcList zip res).map { case (i, r) => new Image(i, r) })))
  }

}
