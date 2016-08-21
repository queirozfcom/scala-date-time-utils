package com.queirozf.utils.date

import java.time.{DayOfWeek => JTDayOfWeek, ZonedDateTime}

import scala.util.{Failure, Success, Try}

private [date] object DateMath {

  import Constants._

  def truncate(base: ZonedDateTime, unitString: String): Try[ZonedDateTime] =
    unitString match {
      case "s" | "S" => Success(calculateTruncation(base, Second))
      case "m" => Success(calculateTruncation(base, Minute))
      case "h" | "H" => Success(calculateTruncation(base, Hour))
      case "d" | "D" => Success(calculateTruncation(base, Day))
      case "w" | "W" => Success(calculateTruncation(base, Week))
      case "M" => Success(calculateTruncation(base, Month))
      case "y" | "Y" => Success(calculateTruncation(base, Year))
      case _ => Failure(new RuntimeException(s"Invalid time unit for date truncation: $unitString"))
    }

  def doMath(base: ZonedDateTime, operationString: String, quantity: Long, unitString: String): Try[ZonedDateTime] = {
    val maybeOperation: Option[Operation] = operationString match {
      case "+" => Some(Addition)
      case "-" => Some(Subtraction)
      case _ => None
    }

    val maybeTimeUnit: Option[TimeUnit] = unitString match {
      case "s" | "S" => Some(Second)
      case "m" => Some(Minute)
      case "h" | "H" => Some(Hour)
      case "d" | "D" => Some(Day)
      case "w" | "W" => Some(Week)
      case "M" => Some(Month)
      case "y" | "Y" => Some(Year)
      case _ => None
    }

    maybeOperation match {
      case Some(operation) => {
        maybeTimeUnit match {
          case Some(timeUnit) => Success(calculateOffset(base, operation, quantity, timeUnit))
          case None => Failure(new RuntimeException(s"Bad Time unit provided for DateTime algebra: $unitString"))
        }
      }
      case None => Failure(new RuntimeException(s"Bad Operation provided for DateTime algebra: $operationString"))
    }
  }

  private def calculateOffset(
                               base: ZonedDateTime,
                               operation: Operation,
                               quantity: Long,
                               timeUnit: TimeUnit): ZonedDateTime =
    operation match {
      case Addition => {
        timeUnit match {
          case Second => base.plusSeconds(quantity)
          case Minute => base.plusMinutes(quantity)
          case Hour => base.plusHours(quantity)
          case Day => base.plusDays(quantity)
          case Week => base.plusWeeks(quantity)
          case Month => base.plusMonths(quantity)
          case Year => base.plusYears(quantity)
        }
      }
      case Subtraction => {
        timeUnit match {
          case Second => base.minusSeconds(quantity)
          case Minute => base.minusMinutes(quantity)
          case Hour => base.minusHours(quantity)
          case Day => base.minusDays(quantity)
          case Week => base.minusWeeks(quantity)
          case Month => base.minusMonths(quantity)
          case Year => base.minusYears(quantity)
        }
      }
    }

  /**
   * Truncates a date to the nearest TimeUnit given.
   *
   * I have not wrapped java.time because  Week, Month and Year aren't supported by Java
   * see method truncatedTo in class LocalTime.java
   *
   * @param base
   * @param timeUnit
   * @return
   */
  private def calculateTruncation(base: ZonedDateTime, timeUnit: TimeUnit): ZonedDateTime =
    timeUnit match {
      case Second => base.withNano(0)
      case Minute => base.withSecond(0).withNano(0)
      case Hour => base.withMinute(0).withSecond(0).withNano(0)
      case Day => base.withHour(0).withMinute(0).withSecond(0).withNano(0)
      case Week => {
        base.getDayOfWeek match {
          case JTDayOfWeek.MONDAY => base.withHour(0).withMinute(0).withSecond(0).withNano(0)
          case JTDayOfWeek.TUESDAY => base.minusDays(1).withHour(0).withMinute(0).withSecond(0).withNano(0)
          case JTDayOfWeek.WEDNESDAY => base.minusDays(2).withHour(0).withMinute(0).withSecond(0).withNano(0)
          case JTDayOfWeek.THURSDAY => base.minusDays(3).withHour(0).withMinute(0).withSecond(0).withNano(0)
          case JTDayOfWeek.FRIDAY => base.minusDays(4).withHour(0).withMinute(0).withSecond(0).withNano(0)
          case JTDayOfWeek.SATURDAY => base.minusDays(5).withHour(0).withMinute(0).withSecond(0).withNano(0)
          case JTDayOfWeek.SUNDAY => base.minusDays(6).withHour(0).withMinute(0).withSecond(0).withNano(0)
        }
      }
      case Month => base.withDayOfMonth(1).withHour(0).withMinute(0).withSecond(0).withNano(0)
      case Year => base.withDayOfYear(1).withHour(0).withMinute(0).withSecond(0).withNano(0)
    }

}
