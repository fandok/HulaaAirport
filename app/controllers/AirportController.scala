package controllers

import javax.inject.{Inject, Singleton}
import anorm._
import play.api.Play.current
import play.api.db.DB
import play.api.mvc._
import play.api.libs.json._
import models._
import models.Airport._

import scala.concurrent.Future

/**
  * Created by msi gs on 6/15/2016.
  */
@Singleton
class AirportController @Inject() extends Controller {

  def timezone(iata: String) = Action.async {
    Future.successful {
      val timezone = Airport.findByID(iata)
      timezone match {
        case None => NotFound("Wrong ID For Timezone")
        case Some(s) => Ok(Json.toJson(Airport.convertTimezone(timezone)))
      }
    }
  }

  def city(iata: String) = Action.async {
    Future.successful {
      val city = Airport.findByID(iata)
      city match {
        case None => NotFound("Wrong ID For City")
        case Some(s) => Ok(Json.toJson(Airport.convertCity(city)))
      }
    }
  }

  def airports(domestic: Boolean) = Action.async {
    Future.successful {
      val airports = Airport.findAll(domestic)
      val airportsDomestic = Airport.convert(airports, domestic)
      Ok(Json.toJson(airportsDomestic))
    }
  }

  def isDomestic(iata: String) = Action.async {
    Future.successful {
      val isDomestic = Airport.findByID(iata)
      isDomestic match {
        case None => NotFound("No Domestic Airports")
        case Some(s) => Ok(Json.toJson(Airport.convertDomestic(isDomestic)))
      }
    }
  }

  def insertData = Action.async { implicit request =>
    Future.successful {
      request.body.asJson match {
        case None => BadRequest("JSON tidak ditemukan")
        case Some(json) => json.validate[Airport] match {
          case s: JsSuccess[Airport] => Ok(Json.toJson(insertHelper(s.get)))
          case e: JsError => BadRequest(e.errors.toString)
        }
      }
    }
  }

  def insertHelper(data: Airport): String = {
    DB.withConnection { implicit c =>
      val list = SQL("SELECT * FROM `airport` WHERE id = {id}").on('id -> data.id).as(Airport.simpleParser *)
      if (list.isEmpty == true) {
        SQL("INSERT INTO `airport`(id,timezone,city,domestic) VALUES({id},{timezone},{city},{domestic})")
          .on('id -> data.id, 'timezone -> data.timezone, 'city -> data.city, 'domestic -> data.domestic)
          .executeInsert()
      } else {
        SQL("UPDATE airport SET `timezone`={timezone},`city`={city},`domestic`={domestic} WHERE `id`={id}")
          .on('id -> data.id, 'timezone -> data.timezone, 'city -> data.city, 'domestic -> data.domestic)
          .executeUpdate()
      }
    }
    "SUCCESS"
  }
}