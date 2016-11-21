package models

import anorm._
import anorm.SqlParser._
import play.api.Play.current
import play.api.db.DB
import play.api.libs.json._
import play.api.libs.functional.syntax._


/**
  * Created by alex on 6/15/16.
  */

case class Airport(id: String,
                   timezone: String,
                   city: Option[String],
                   domestic: Option[Boolean])

object Airport {
  implicit val formatAirport: Format[Airport] = (
    (__ \ "id").format[String] and
      (__ \ "timezone").format[String] and
      (__ \ "city").formatNullable[String] and
      (__ \ "domestic").formatNullable[Boolean]
    )(Airport.apply, unlift(Airport.unapply))

  implicit val formatTimezone: Format[Timezone] = (
    (__ \ "id").format[String] and
      (__ \ "timezone").format[String]
    ) (Timezone.apply, unlift(Timezone.unapply))

  implicit val formatCity: Format[City] = (
    (__ \ "airport").format[String] and
      (__ \ "city").formatNullable[String]
    ) (City.apply, unlift(City.unapply))

  implicit val formatIsDomestic: Format[IsDomestic] = (
    (__ \ "airport").format[String] and
      (__ \ "isDomestic").format[Boolean]
    ) (IsDomestic.apply, unlift(IsDomestic.unapply))

  implicit val formatDomesticAirport: Format[DomesticAirports] = (
    (__ \ "id").format[Seq[String]] and
      (__ \ "domestic").format[Boolean]
    )(DomesticAirports.apply, unlift(DomesticAirports.unapply))

  val simpleParser = {
    get[String]("id") ~
      get[String]("timezone") ~
      get[Option[String]]("city") ~
      get[Option[Boolean]]("domestic") map {
      case id~timezone~city~domestic => Airport(id, timezone, city, domestic)
    }
  }

  def convertTimezone(airports: Option[Airport]) : Timezone = {
    Timezone(airports.map(a => a.id).head, airports.map(a => a.timezone).head)
  }

  def convertCity(airports: Option[Airport]) : City = {
    City(airports.map(a => a.id).head, airports.map(a => a.city).head)
  }

  def convertDomestic(airports: Option[Airport]) : IsDomestic = {
    airports match {
      case None => throw new Exception{"Gabisa"}
      case Some(airport) => IsDomestic(airport.id,airport.domestic.getOrElse(false))
    }
  }

  def convert(airports: Seq[Airport], domestic: Boolean): DomesticAirports = {
    val listOfAirports = airports.map(a => a.id)
    DomesticAirports(listOfAirports, domestic)
  }


  def findByID(id: String): Option[Airport] = DB.withConnection { implicit c =>
    val list = SQL("SELECT * FROM airport WHERE airport.id = {id}")
      .on('id -> id)
      .as(Airport.simpleParser *)

    list match {
      case Nil => None
      case head :: tail => Some(head)
      case lst => Some(lst.head)
    }
  }

  def findAll(domestic: Boolean): Seq[Airport] = DB.withConnection { implicit c =>
    val list = SQL("SELECT * FROM `airport` WHERE airport.domestic = {domestic}")
      .on('domestic -> domestic)
      .as(Airport.simpleParser *)

    list
  }

  case class Timezone(id: String, timezone: String)
  case class City(id: String, city: Option[String])
  case class IsDomestic(id: String, domestic: Boolean)
  case class DomesticAirports(iatas: Seq[String], isDomestic: Boolean)
}