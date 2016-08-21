package com.queirozf.utils

/**
  * Created by felipe on 20/08/16.
  */
package object date {

  object Constants {

    sealed trait TimeUnit

    case object Second extends TimeUnit

    case object Minute extends TimeUnit

    case object Hour extends TimeUnit

    case object Day extends TimeUnit

    case object Week extends TimeUnit

    case object Month extends TimeUnit

    case object Year extends TimeUnit

    sealed trait Operation

    case object Addition extends Operation

    case object Subtraction extends Operation

  }

  sealed case class ValidDateString(value: String)

}
