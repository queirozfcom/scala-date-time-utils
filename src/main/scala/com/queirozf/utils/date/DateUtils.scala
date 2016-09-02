package com.queirozf.utils.date

import java.sql.{Date => SQLDate, Time => SQLTime, Timestamp => SQLTimestamp}
import java.time._
import java.time.format.DateTimeFormatter

import scala.util.{Failure, Success, Try}

object DateUtils {

  private def displayZoneIdStr: String = "America/Sao_Paulo"

  private def fallbackSourceZoneIdStr: String = "UTC"

  def parse(dateString: String): Try[ZonedDateTime] = parseString(dateString)

  def toString(zdt: ZonedDateTime): String = {

    val formatter = DateTimeFormatter.ISO_OFFSET_DATE_TIME

    val displayZoneId = ZoneId.of(displayZoneIdStr)

    zdt.withZoneSameInstant(displayZoneId).format(formatter)

  }

  /**
   *
   * @param ts
   * @param localTimeZone if None, the timestamp will be tagged with the default fallback time zone
   * @return
   */
  def toString(ts: SQLTimestamp, localTimeZone: Option[ZoneId]): String = {
    val ldt = ts.toLocalDateTime
    val displayZoneId = ZoneId.of(displayZoneIdStr)


    val zoneId = localTimeZone match {
      case Some(id) => id
      case None => ZoneId.of(fallbackSourceZoneIdStr)
    }

    val zdt = ldt.atZone(zoneId).withZoneSameInstant(displayZoneId)
    zdt.format(DateTimeFormatter.ISO_OFFSET_DATE_TIME)

  }

  /**
   *
   * @param t
   * @param localTimeZone if None, the timestamp will be tagged with the default fallback time zone
   * @return
   */
  def toString(t: SQLTime, localTimeZone: Option[ZoneId]): String = {
    val lt = t.toLocalTime

    val displayZoneId = ZoneId.of(displayZoneIdStr)

    val zoneId = localTimeZone match {
      case Some(id) => id
      case None => ZoneId.of(fallbackSourceZoneIdStr)
    }

    val defaultZoneOffset = zoneId.getRules.getOffset(Instant.now())

    val localZoneOffset = displayZoneId.getRules.getOffset(Instant.now())

    lt.atOffset(defaultZoneOffset).withOffsetSameInstant(localZoneOffset).format(DateTimeFormatter.ISO_OFFSET_TIME)

  }

  def toString(d: SQLDate): String = {
    val ld = d.toLocalDate
    val formatter = DateTimeFormatter.ISO_DATE
    ld.format(formatter)
  }

  /**
   * If the input is a valid zoned date, then format it using our display format. Otherwise
   * just return the original string.
   *
   * @param str
   * @return
   */
  def formatIfDate(str: String): String = parse(str) match {
    case Success(zdt) => toString(zdt)
    case Failure(_) => str
  }

  /**
    * ZonedDateTime class has no method toEpochMillis, only toEpochSecond
    *
    * @param zdt
    * @return
    */
  def toEpochMillis(zdt: ZonedDateTime) : Long = {
    val numSeconds = zdt.toEpochSecond
    val extraMillis = zdt.getNano / 1000000

    numSeconds * 1000 + extraMillis
  }


  private val nowPattern = """(now|NOW)"""

  private val mathArgsPattern = """([+-])(\d+)?([sSmMhHdDyYwW])"""

  private val truncationPattern = """(/[sSmMhHdDyYwW])"""

  // there may be multiple operations, e.g. in now+5d-10W+35m
  private val fullPattern =
    raw"""^$nowPattern($mathArgsPattern)*$truncationPattern?$$"""

  type MathArgs = (String, Long, String)

  type TruncationArg = String

  private def parseString(str: String): Try[ZonedDateTime] = {

    if (str.matches(fullPattern)) {

      val now = ZonedDateTime.now()

      // we know it's a valid date string otherwise it wouldn't match the fullPattern
      val validDate = ValidDateString(str)

      val mathOperations = extractMathOperations(validDate)
      val maybeTruncationUnit = extractTruncationValue(validDate)

      // apply all addition and subraction operations, if any
      val correctDate: ZonedDateTime = mathOperations.foldLeft(now) { (carry, mathOper) =>
        DateMath.doMath(carry, mathOper._1, mathOper._2, mathOper._3).get
      }

      maybeTruncationUnit match {
        case Some(truncationUnit) => DateMath.truncate(correctDate, truncationUnit)
        case None => Success(correctDate) //no truncation needed
      }

    } else tryISOParse(str).orElse(tryEpochParse(str))
  }

  /**
   * Tries to convert a date string into a java.time.ZonedDateTime
   *
   * @param str an ISO-conformant date string
   * @return a zoned datetime, in case it was possible to parse the input string
   */
  private def tryISOParse(str: String): Try[ZonedDateTime] = {
    Try(ZonedDateTime.parse(str)) match {
      case Success(zdt) => Success(zdt)
      case Failure(_) => {
        Try(LocalDateTime.parse(str)) match {
          case Success(ldt) => Success(ldt.atZone(ZoneOffset.UTC))
          case Failure(_) => {
            Try(LocalDate.parse(str)) match {
              case Success(ld) => Success(ld.atStartOfDay(ZoneOffset.UTC))
              case Failure(e) => Failure(e)
            }
          }
        }
      }
    }
  }

  private def tryEpochParse(str:String) : Try[ZonedDateTime] = {
    for{
      asLong <- Try(str.toLong)
      asZDT <- {

        // if the string has 12 digits or more, assume it's milliseconds
        // http://stackoverflow.com/a/23982005/436721

        if(str.length >= 12) Try(Instant.ofEpochMilli(asLong))
        else Try(Instant.ofEpochSecond(asLong))
      }
    } yield asZDT.atZone(ZoneId.of("UTC"))
  }

  private def extractMathOperations(str: ValidDateString): Seq[MathArgs] = {

    val matches = mathArgsPattern.r.findAllMatchIn(str.value)

    matches.toSeq.map { re =>
      val operation = re.group(1) // either + or -
    val quantity = Option(re.group(2)) match {
        case Some(num) => num.toLong
        case None => 1L
      }
      val unit = re.group(3)

      (operation, quantity, unit)
    }
  }

  private def extractTruncationValue(str: ValidDateString): Option[TruncationArg] = {
    val truncationAtTheEnd = raw"""$truncationPattern$$"""
    val maybeSlashAndUnit = truncationAtTheEnd.r.findFirstIn(str.value)

    maybeSlashAndUnit match {
      case Some(slashAndUnit) => {
        val unit = slashAndUnit.split('/')(1)
        Some(unit)
      }
      case None => None
    }
  }
}

