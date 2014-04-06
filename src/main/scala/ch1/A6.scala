package ch1

object A6 extends App {

  def newif(p: Boolean, f1: => Double, f2: => Double): Double = {
    p match {
      case true => f1
      case false => f2
    }
  }

  def sqrt(x: Double): Double = {

    def good(guess: Double, nextguess: Double): Boolean = {
      Math.abs(guess - nextguess) <  1e-15
    }

    def improve(guess: Double): Double = {
      average(guess, x / guess)
    }

    def average(x: Double, y: Double) = {
      (x + y) / 2
    }

    def sqrtIter(guess: Double): Double = {
      val nextguess = improve(guess);
      if (good(guess, nextguess)) {
        guess
      } else {
        sqrtIter(nextguess)
      }
    }

    sqrtIter(1.0)
  }

  println( sqrt( 0.2 ))
  println( Math.sqrt( 0.2))
}