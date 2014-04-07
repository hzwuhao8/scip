package ch1

import org.slf4j.LoggerFactory
import scala.annotation.tailrec
/**
 * Page 30
 */
object A16a extends App {
  val log = LoggerFactory.getLogger(A16a.getClass())

  var count = 1
  def expt(b: Int, n: Int): Int = {
    count += 1
    if (n == 0) {
      1
    } else {
      b * expt(b, n - 1)
    }
  }
  for (n <- 1 to 10) {
    count = 1
    log.info("expt(2,{})={},count={}", n.toString, expt(2, n).toString, count.toString)
  }

}

object A16b extends App {
  val log = LoggerFactory.getLogger(A16b.getClass())

  var count = 1
  def expt(b: Int, n: Int): Int = {
    @tailrec
    def exptInter(b: Int, n: Int, p: Int): Int = {
      count += 1
      if (n == 0) {
        p
      } else {
        exptInter(b, n - 1, b * p)
      }
    }
    exptInter(b, n, 1)
  }
  for (n <- 1 to 10) {
    count = 1
    log.info("expt(2,{})={},count={}", n.toString, expt(2, n).toString, count.toString)
  }

}
object A16c extends App {
  val log = LoggerFactory.getLogger(A16c.getClass())

  var count = 1
  def expt(b: Int, n: Int): Int = {
    count += 1
    n match {
      case 0 => 1
      case n if (n % 2 == 0) =>
        val t = expt(b, n / 2); t * t
      case n => val t = expt(b, (n - 1) / 2); b * t * t
    }

  }
  for (n <- 1 to 10) {
    count = 1
    log.info("expt(2,{})={},count={}", n.toString, expt(2, n).toString, count.toString)
  }

}

object A16d extends App {
  val log = LoggerFactory.getLogger(A16d.getClass())

  var count = 1

  @tailrec
  def expt(b: Int, n: Int, a: Int): Int = {
    log.debug("b={},n={},a={}", b.toString, n.toString , a.toString)
    count += 1
    n match {
      case 0 => a
      case 1 => a*b
      case n if (n % 2 == 0) => expt(b * b, n / 2, a)
      case n if (n % 2 == 1) => expt(b * b, (n - 1) / 2, a * b)
    }

  }
  for (n <- 0 to 10) {
    count = 1
    val r = expt(3, n, 1)
    log.info("expt(3,{})={},count={} , eq={}", n.toString, r.toString, count.toString , (r == Math.pow(3,n)).toString )
  }

}