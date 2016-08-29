import java.time.temporal.ChronoUnit._
import java.time.{Duration, Instant, ZoneId, ZonedDateTime => ZDT}

import org.scalatest.{FlatSpec, MustMatchers}
import com.queirozf.utils.date.{DateUtils => Utils}

class DateUtilsSpec extends FlatSpec with MustMatchers {

  val zeroSeconds = Duration.ofSeconds(0)
  val zeroMinutes = Duration.ofMinutes(0)
  val zeroDays = Duration.ofDays(0)

  def assertCloseEnough(t1: ZDT, t2: ZDT) = {

    // the parser takes a few milliseconds to parse dates so now() called before parsing will never
    // match now() after the parser has finished parsing, so 200 milliseconds should suffice
    if (Duration.between(t1, t2).toMillis <= Duration.ofMillis(200).toMillis)
      ()
    else {
      val dur = Duration.between(t1, t2)
      throw new AssertionError(s"Not close enough. Actual Duration between DateTimes compared: $dur")
    }
  }

  it should "compile" in {
    assertCompiles("com.queirozf.utils.date.DateUtils")
  }

  it should "parse now" in {

    assertCloseEnough(ZDT.now(), Utils.parse("now").get)

    assertCloseEnough(ZDT.now(), Utils.parse("NOW").get)
  }

  it should "parse now-based single addition" in {
    assertCloseEnough(ZDT.now().plusSeconds(32), Utils.parse("now+32s").get)

    assertCloseEnough(ZDT.now().plusSeconds(32), Utils.parse("now+32S").get)

    assertCloseEnough(ZDT.now().plusMinutes(88), Utils.parse("now+88m").get)

    assertCloseEnough(ZDT.now().plusHours(38), Utils.parse("now+38h").get)

    assertCloseEnough(ZDT.now().plusHours(38), Utils.parse("now+38H").get)

    assertCloseEnough(ZDT.now().plusDays(76), Utils.parse("now+76d").get)

    assertCloseEnough(ZDT.now().plusDays(1), Utils.parse("now+d").get)

    assertCloseEnough(ZDT.now().plusDays(1), Utils.parse("now+D").get)

    assertCloseEnough(ZDT.now().plusDays(76), Utils.parse("now+76D").get)

    assertCloseEnough(ZDT.now().plusWeeks(32), Utils.parse("now+32w").get)

    assertCloseEnough(ZDT.now().plusWeeks(1), Utils.parse("now+W").get)

    assertCloseEnough(ZDT.now().plusWeeks(1), Utils.parse("now+1W").get)

    assertCloseEnough(ZDT.now().plusMonths(2), Utils.parse("now+2M").get)

    assertCloseEnough(ZDT.now().plusYears(2), Utils.parse("now+2y").get)

  }

  it should "parse now-based single subtraction" in {
    assertCloseEnough(ZDT.now().minusSeconds(32), Utils.parse("now-32s").get)

    assertCloseEnough(ZDT.now().minusSeconds(32), Utils.parse("now-32S").get)

    assertCloseEnough(ZDT.now().minusMinutes(88), Utils.parse("now-88m").get)

    assertCloseEnough(ZDT.now().minusHours(38), Utils.parse("now-38h").get)

    assertCloseEnough(ZDT.now().minusHours(38), Utils.parse("now-38H").get)

    assertCloseEnough(ZDT.now().minusDays(76), Utils.parse("now-76D").get)

    assertCloseEnough(ZDT.now().minusWeeks(312), Utils.parse("now-312w").get)

    assertCloseEnough(ZDT.now().minusWeeks(2), Utils.parse("now-2W").get)

    assertCloseEnough(ZDT.now().minusWeeks(1), Utils.parse("now-1W").get)

    assertCloseEnough(ZDT.now().minusWeeks(1), Utils.parse("now-W").get)

    assertCloseEnough(ZDT.now().minusWeeks(1), Utils.parse("now-w").get)

    assertCloseEnough(ZDT.now().minusMonths(2), Utils.parse("now-2M").get)

    assertCloseEnough(ZDT.now().minusYears(2), Utils.parse("now-2y").get)
  }

  it should "parse multiple operations" in {
    assertCloseEnough(ZDT.now(), Utils.parse("now-W+W").get)

    assertCloseEnough(ZDT.now(), Utils.parse("now-2w+W+1w").get)

    assertCloseEnough(ZDT.now().plusDays(7), Utils.parse("now+8d-24h").get)
  }

  it should "parse now-based truncation" in {
    // elasticsearch call this "rounding" but it's wrong!
    // rounding approximates to the closest integer or whole value, but this operation sets a timestamp
    // to its closest *previous* whole value. It never selects a value in the future.

    assertCloseEnough(ZDT.now().truncatedTo(SECONDS), Utils.parse("now/s").get)

    assertCloseEnough(ZDT.now().truncatedTo(SECONDS), Utils.parse("now/S").get)

    assertCloseEnough(ZDT.now().truncatedTo(MINUTES), Utils.parse("now/m").get)

    assertCloseEnough(ZDT.now().truncatedTo(HOURS), Utils.parse("now/h").get)

    assertCloseEnough(ZDT.now().truncatedTo(HOURS), Utils.parse("now/H").get)

    assertCloseEnough(ZDT.now().truncatedTo(DAYS), Utils.parse("now/d").get)

    assertCloseEnough(ZDT.now().truncatedTo(DAYS), Utils.parse("now/D").get)

    assertCloseEnough(ZDT.now().withDayOfMonth(1).withHour(0).withSecond(0).withNano(0), Utils.parse("now/M").get)

    assertCloseEnough(ZDT.now().withDayOfYear(1).withDayOfMonth(1).truncatedTo(HOURS), Utils.parse("now/y").get)

    assertCloseEnough(ZDT.now().withDayOfYear(1).withDayOfMonth(1).truncatedTo(HOURS), Utils.parse("now/Y").get)

  }

  it should "parse now-based operations with truncation" in {

    assertCloseEnough(ZDT.now().plusDays(10).truncatedTo(SECONDS), Utils.parse("now+10d/s").get)

    assertCloseEnough(ZDT.now().minusHours(1).truncatedTo(SECONDS), Utils.parse("now-h/S").get)

    assertCloseEnough(ZDT.now().minusMinutes(70).truncatedTo(MINUTES), Utils.parse("now-70m/m").get)

    assertCloseEnough(ZDT.now().plusDays(21).truncatedTo(HOURS), Utils.parse("now+21D/h").get)

    assertCloseEnough(ZDT.now().minusHours(2).truncatedTo(DAYS), Utils.parse("now-2h/d").get)

    assertCloseEnough(ZDT.now().plusYears(4).withDayOfMonth(1).withHour(0).withMinute(0).withSecond(0).withNano(0), Utils.parse("now+4y/M").get)

    assertCloseEnough(ZDT.now().plusMonths(231).withDayOfYear(1).withHour(0).withMinute(0).withSecond(0).withNano(0), Utils.parse("now+231M/y").get)

  }

  it should "parse full dates as well" in {


    //when no timezone given, it assumes UTC (i.e. z-suffix)

    val fullDate = ZDT.parse("2015-04-26T00:00:58.000Z")
    val fullDateStartOfDay = ZDT.parse("2015-04-26T00:00:00.000Z")

    Utils.parse("2015-04-26T00:00:58.000Z").get must equal(fullDate)
    Utils.parse("2015-04-26T00:00:58").get must equal(fullDate)
    Utils.parse("2015-04-26T00:00:58Z").get must equal(fullDate)
    Utils.parse("2015-04-26").get must equal(fullDateStartOfDay)

  }

  it should "parse epoch milliseconds, assuming UTC" in {

    val millisStr = "1472495308075"
    val millisLong = 1472495308075L

    // this is the equivalent form, manually created
    val manuallyCreated = Instant.ofEpochMilli(millisLong).atZone(ZoneId.of("UTC"))
    val parsed = Utils.parse(millisStr)

    manuallyCreated must equal(parsed.get)

  }

  it should "parse epoch seconds, assuming UTC" in {
    val secondsStr = "1472495308"
    val secondsLong = 1472495308L

    // this is the equivalent form, manually created
    val manuallyCreated = Instant.ofEpochSecond(secondsLong).atZone(ZoneId.of("UTC"))
    val parsed = Utils.parse(secondsStr)

    manuallyCreated must equal(parsed.get)
  }

}
