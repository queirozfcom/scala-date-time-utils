package scala.date

import java.time.{Duration, ZonedDateTime => ZDT}
import java.time.temporal.ChronoUnit._
import org.scalatest.{MustMatchers, TryValues, Matchers, FlatSpec}
import helpers.date.{
DateHelper => Helper
}

/**
 * Created by falmeida@3elos on 8/26/15.
 */
class DateHelperSpec extends FlatSpec with MustMatchers {

  val zeroSeconds = Duration.ofSeconds(0)
  val zeroMinutes = Duration.ofMinutes(0)
  val zeroDays = Duration.ofDays(0)

  def assertCloseEnough(t1: ZDT, t2: ZDT) = {

    // the parser takes a few milliseconds to parse dates so now() called before parsing will never
    // match now() after the parser has finished parsing, so 100 milliseconds should suffice
    if (Duration.between(t1, t2).toMillis <= Duration.ofMillis(100).toMillis)
      ()
    else {
      val dur = Duration.between(t1, t2)
      throw new AssertionError(s"Not close enough. Actual Duration between DateTimes compared: $dur")
    }
  }

  it should "compile" in {
    assertCompiles("helpers.date.DateHelper")
  }

  it should "parse now" in {

    assertCloseEnough(ZDT.now(), Helper.parse("now").get)

    assertCloseEnough(ZDT.now(), Helper.parse("NOW").get)
  }

  it should "parse now-based single addition" in {
    assertCloseEnough(ZDT.now().plusSeconds(32), Helper.parse("now+32s").get)

    assertCloseEnough(ZDT.now().plusSeconds(32), Helper.parse("now+32S").get)

    assertCloseEnough(ZDT.now().plusMinutes(88), Helper.parse("now+88m").get)

    assertCloseEnough(ZDT.now().plusHours(38), Helper.parse("now+38h").get)

    assertCloseEnough(ZDT.now().plusHours(38), Helper.parse("now+38H").get)

    assertCloseEnough(ZDT.now().plusDays(76), Helper.parse("now+76d").get)

    assertCloseEnough(ZDT.now().plusDays(1), Helper.parse("now+d").get)

    assertCloseEnough(ZDT.now().plusDays(1), Helper.parse("now+D").get)

    assertCloseEnough(ZDT.now().plusDays(76), Helper.parse("now+76D").get)

    assertCloseEnough(ZDT.now().plusWeeks(32), Helper.parse("now+32w").get)

    assertCloseEnough(ZDT.now().plusWeeks(1), Helper.parse("now+W").get)

    assertCloseEnough(ZDT.now().plusWeeks(1), Helper.parse("now+1W").get)

    assertCloseEnough(ZDT.now().plusMonths(2), Helper.parse("now+2M").get)

    assertCloseEnough(ZDT.now().plusYears(2), Helper.parse("now+2y").get)

  }

  it should "parse now-based single subtraction" in {
    assertCloseEnough(ZDT.now().minusSeconds(32), Helper.parse("now-32s").get)

    assertCloseEnough(ZDT.now().minusSeconds(32), Helper.parse("now-32S").get)

    assertCloseEnough(ZDT.now().minusMinutes(88), Helper.parse("now-88m").get)

    assertCloseEnough(ZDT.now().minusHours(38), Helper.parse("now-38h").get)

    assertCloseEnough(ZDT.now().minusHours(38), Helper.parse("now-38H").get)

    assertCloseEnough(ZDT.now().minusDays(76), Helper.parse("now-76D").get)

    assertCloseEnough(ZDT.now().minusWeeks(312), Helper.parse("now-312w").get)

    assertCloseEnough(ZDT.now().minusWeeks(2), Helper.parse("now-2W").get)

    assertCloseEnough(ZDT.now().minusWeeks(1), Helper.parse("now-1W").get)

    assertCloseEnough(ZDT.now().minusWeeks(1), Helper.parse("now-W").get)

    assertCloseEnough(ZDT.now().minusWeeks(1), Helper.parse("now-w").get)

    assertCloseEnough(ZDT.now().minusMonths(2), Helper.parse("now-2M").get)

    assertCloseEnough(ZDT.now().minusYears(2), Helper.parse("now-2y").get)
  }

  it should "parse multiple operations" in {
    assertCloseEnough(ZDT.now(), Helper.parse("now-W+W").get)

    assertCloseEnough(ZDT.now(), Helper.parse("now-2w+W+1w").get)

    assertCloseEnough(ZDT.now().plusDays(7), Helper.parse("now+8d-24h").get)
  }

  it should "parse now-based truncation" in {
    // elasticsearch call this "rounding" but it's wrong!
    // rounding approximates to the closest integer or whole value, but this operation sets a timestamp
    // to its closest *previous* whole value. It never selects a value in the future.

    assertCloseEnough(ZDT.now().truncatedTo(SECONDS), Helper.parse("now/s").get)

    assertCloseEnough(ZDT.now().truncatedTo(SECONDS), Helper.parse("now/S").get)

    assertCloseEnough(ZDT.now().truncatedTo(MINUTES), Helper.parse("now/m").get)

    assertCloseEnough(ZDT.now().truncatedTo(HOURS), Helper.parse("now/h").get)

    assertCloseEnough(ZDT.now().truncatedTo(HOURS), Helper.parse("now/H").get)

    assertCloseEnough(ZDT.now().truncatedTo(DAYS), Helper.parse("now/d").get)

    assertCloseEnough(ZDT.now().truncatedTo(DAYS), Helper.parse("now/D").get)

    assertCloseEnough(ZDT.now().withDayOfMonth(1).withHour(0).withSecond(0).withNano(0), Helper.parse("now/M").get)

    assertCloseEnough(ZDT.now().withDayOfYear(1).withDayOfMonth(1).truncatedTo(HOURS), Helper.parse("now/y").get)

    assertCloseEnough(ZDT.now().withDayOfYear(1).withDayOfMonth(1).truncatedTo(HOURS), Helper.parse("now/Y").get)

  }

  it should "parse now-based operations with truncation" in {

    assertCloseEnough(ZDT.now().plusDays(10).truncatedTo(SECONDS), Helper.parse("now+10d/s").get)

    assertCloseEnough(ZDT.now().minusHours(1).truncatedTo(SECONDS), Helper.parse("now-h/S").get)

    assertCloseEnough(ZDT.now().minusMinutes(70).truncatedTo(MINUTES), Helper.parse("now-70m/m").get)

    assertCloseEnough(ZDT.now().plusDays(21).truncatedTo(HOURS), Helper.parse("now+21D/h").get)

    assertCloseEnough(ZDT.now().minusHours(2).truncatedTo(DAYS), Helper.parse("now-2h/d").get)

    assertCloseEnough(ZDT.now().plusYears(4).withDayOfMonth(1).withHour(0).withMinute(0).withSecond(0).withNano(0), Helper.parse("now+4y/M").get)

    assertCloseEnough(ZDT.now().plusMonths(231).withDayOfYear(1).withHour(0).withMinute(0).withSecond(0).withNano(0), Helper.parse("now+231M/y").get)

  }

  it should "parse full dates as well" in {


    //when no timezone given, it assumes UTC (i.e. z-suffix)

    val fullDate = ZDT.parse("2015-04-26T00:00:58.000Z")
    val fullDateStartOfDay = ZDT.parse("2015-04-26T00:00:00.000Z")

    Helper.parse("2015-04-26T00:00:58.000Z").get must equal(fullDate)
    Helper.parse("2015-04-26T00:00:58").get must equal(fullDate)
    Helper.parse("2015-04-26T00:00:58Z").get must equal(fullDate)
    Helper.parse("2015-04-26").get must equal(fullDateStartOfDay)

  }

}
