package controllers

import dispatch._

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
import scala.concurrent.{blocking, Future, Await}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
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
/*
  def reverseSearchGoogle2(imgUrl: String) = {
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
*/
  def reverseSearchGoogle(imgUrl: String) : Future[String] = {
    /* Ask Google reverse image search and return future of result HTML */
    val userAgent: String = "Mozilla/5.0 (X11; Linux i686) AppleWebKit/537.36 (KHTML, like Gecko) Ubuntu Chromium/37.0.2062.120 Chrome/37.0.2062.120 Safari/537.36"
    val gUrl = url("http://www.google.com/searchbyimage?image_url=" + imgUrl) <:< Map("User-Agent" -> userAgent)
    val resFuture = Http.configure(_ setFollowRedirects true)(gUrl OK as.String)
    println("resFuture")
    resFuture onComplete {
      case Success(content) => {
	println("success")
      }
      case Failure(t) => {
	println("Error: " + t.getMessage)
      }
    }
    return resFuture
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
      print(urlRedirect)
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
    var imgSrcs = new HashSet[String] // @TODO: inherit from SortedSet
    while (images.hasNext()) {
      val src = images.next().attr("src")
      // @TODO: Handle local files: append http://domain
      val protocols = Array("http://", "https://", "ftp://") // @TODO: Move above
      if (protocols.foldLeft(false) { (x, y) => x || src.startsWith(y) } ) {
	imgSrcs += src
      }
    }
    imgNum = imgSrcs.size

    /* Multitasking with implicit thread pool */
    val resultFutures: List[Future[String]] = imgSrcs.toList.map {
      imgSrc => reverseSearchGoogle(imgSrc)
    }
    val futureResult: Future[List[String]] = Future.sequence(resultFutures)
    Await.result(futureResult, 90 seconds) // @TODO: manage blocking more wisely, maybe add exception handler

    Ok("ok")
  }

  def results(url: String) = Action {
    Ok(views.html.results(url))
  }

}
