package ch1

import org.slf4j.LoggerFactory
import scala.annotation.tailrec

object A20 extends App {
  val log = LoggerFactory.getLogger(A20.getClass())

  var count1 = 0
  @tailrec
  def gcd1(a: Int, b: Int): Int = {
    count1 += 1
    if (b == 0) {
      a
    } else {
      gcd1(b, (a % b))
    }
  }
  val a = 20680000
  val b = 4050002
  log.info("gcd({},{})={} , count={}", a.toString, b.toString, (gcd1(a, b)).toString, count1.toString)

  @tailrec
  var count2=0
  def gcd2(a:  => Int, b: => Int): Int = {
    count2 += 1
    if (b == 0) {
      a
    } else {
      gcd2(b, (a % b))
    }
  }
  
  log.info("gcd({},{})={} , count={}", a.toString, b.toString, (gcd2(a, b)).toString, count2.toString)
}