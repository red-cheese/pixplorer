package controllers

import dispatch._, Defaults._

/* Scala webscraper to extract image src's from web page */
import net.ruippeixotog.scalascraper.browser.Browser
import net.ruippeixotog.scalascraper.dsl.DSL._
import net.ruippeixotog.scalascraper.dsl.DSL.Extract._
import net.ruippeixotog.scalascraper.dsl.DSL.Parse._

import org.jsoup.nodes.Element
import org.jsoup.select.Elements
import org.jsoup.Jsoup
import play.api._
import play.api.data._
import play.api.data.Forms._
import play.api.mvc._
import scala.collection.concurrent._
import scala.collection.mutable._
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.{Success, Failure}

case class URL(url: String)

object Application extends Controller {

  val urlForm = Form(
    mapping(
      "url" -> play.api.data.Forms.text
    )(URL.apply)(URL.unapply)
  )

  val browser = new Browser

  /* This is an ad hoc implementation of concurrent multimap to capture our async reverse image search results */
  var mmap: TrieMap[String, TrieMap[String, String]] = new TrieMap
  var imgNum: Int = 0
  var urlRedirect: String = "/"

  def index = Action {
    Ok(views.html.index(urlForm))
  }

  def reverseSearchGoogle(imgUrl: String) = {
    mmap += imgUrl -> new TrieMap[String, String]
    /* Ask Google reverse image search */
    val gUrlSrc = url("http://www.google.com/searchbyimage?image_url=" + imgUrl)
    val gUrl = gUrlSrc <:< Map("User-Agent" -> "Mozilla/5.0 (X11; Linux i686) AppleWebKit/537.36 (KHTML, like Gecko) Ubuntu Chromium/37.0.2062.120 Chrome/37.0.2062.120 Safari/537.36")
    val response: Future[String] = Http.configure(_ setFollowRedirects true)(gUrl OK as.String)
    response onComplete { // Promises are everywhere!
      case Success(content) => {
	val matchElements: Elements = Jsoup.parse(content) >> elements("h3.r a")
	val matches: java.util.Iterator[Element] = matchElements.iterator()
	while (matches.hasNext()) {
	  val href = matches.next().attr("href")
	  mmap.get(imgUrl).get += href -> href
	}
	imgNum -= 1
	renderResultsIfReady
      }
      case Failure(t) => {
        println("An error has occurred: " + t.getMessage)
	imgNum -= 1
	renderResultsIfReady
      }
    }
  }

  def flush = {
    mmap = new TrieMap
    imgNum = 0
  }

  def renderResultsIfReady = {
    if (imgNum == 0) {
      println(mmap)
      flush
      print("flushed")
      Redirect(routes.Application.results(urlRedirect))
    }
  }

  def pixplorer = Action { implicit request =>
    val urlData = urlForm.bindFromRequest.get
    val url = urlData.url
    urlRedirect = url

    /* Pixplore! */

    // @TODO: URL must be valid (http://www...)
    val doc = browser.get(url)
    val imgElements: Elements = doc >> elements("img")
    val images: java.util.Iterator[Element] = imgElements.iterator()
    var imgSrcs = new HashSet[String]
    while (images.hasNext()) {
      val src = images.next().attr("src")
      // @TODO: Handle local files: append http://domain
      val protocols = Array("http://", "https://", "ftp://") // @TODO: Move above
      if (protocols.foldLeft(false) { (x, y) => x || src.startsWith(y) } ) {
	imgSrcs += src
      }
    }
    imgNum = imgSrcs.size
    println(imgNum)
    // @TODO: Add multithreading
    imgSrcs.foreach(reverseSearchGoogle)
    Ok("ok")
  }

  def results(url: String) = Action {
    Ok(views.html.results(url))
  }

}
