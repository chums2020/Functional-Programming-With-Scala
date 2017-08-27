/**
  * Created by ylin on 7/1/2017.
  */
sealed trait NoteName
case object A extends NoteName
case object B extends NoteName
case object C extends NoteName
case object D extends NoteName

sealed trait Symbol
case class Note(name: NoteName, duration: String, octave: Int) extends Symbol
case class Rest(duration: String) extends Symbol

def nonExhaustiveDuration(symbol: Symbol): String =
  symbol match {
    case Note(name, duration, octave) => duration
    case Rest(duration) => duration
  }

