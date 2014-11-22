package controllers

import play.api._
import play.api.data._
import play.api.data.Forms._
import play.api.mvc._

case class URL(url: String)

object Application extends Controller {

  val urlForm = Form(
    mapping(
      "url" -> text
    )(URL.apply)(URL.unapply)
  )

  def index = Action {
    Ok(views.html.index(urlForm))
  }

  def pixplorer = Action { implicit request =>
    var urlData = urlForm.bindFromRequest.get

    // Pixplore!

    // Redirect after POST
    Redirect(routes.Application.results(urlData.url))
  }

  def results(url: String) = Action {
    Ok(views.html.results(url))
  }

}
