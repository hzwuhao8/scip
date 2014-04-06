package ch1
import org.slf4j.Logger
import org.slf4j.LoggerFactory

object A6 extends App {
  val log = LoggerFactory.getLogger(A6.getClass())

  def newif(p: Boolean, f1: => Double, f2: => Double): Double = {
    p match {
      case true => f1
      case false => f2
    }
  }

  def newton(x: Double,   improve: Double => Double): Double = {

    def good(guess: Double, nextguess: Double): Boolean = {
      Math.abs(guess - nextguess) / guess < 1e-15
    }
    
    def newtonInter(guess: Double): Double = {
      val nextguess = improve(guess);

      if (good(guess, nextguess)) {
        (guess + nextguess) / 2
      } else {
        newtonInter(nextguess)
      }
    }
    
    newtonInter(1.0)
  }

  def sqrt(x: Double): Double = {

    def improve(guess: Double): Double = {
      def average(x: Double, y: Double) = {
        (x + y) / 2
      }
      average(guess, x / guess)
    }
    newton(x, improve)

  }
  
  def cube(x: Double): Double = {
     def improve(guess: Double): Double = {
      (( x / (guess*guess)) + 2*guess)/3
       
    }
    newton(x, improve)
  }
  val x = 27
  log.info("sqrt( {} )={} ", x, sqrt(x))
  log.info("Math.sqrt( {} )={}", x, Math.sqrt(x))
  
 log.info("cube( {} )={} ", x,   (cube(x)) )
  log.info("Math.pow( {} )={}", x, Math.pow(x , 1/3.0) )
}