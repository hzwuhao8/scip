package ch1
import org.slf4j.LoggerFactory
import scala.annotation.tailrec
/**
 * Page 29
 */
object A15 extends App {
  val log = LoggerFactory.getLogger(A15.getClass())

  var count = 1
  def cube(y: Double): Double = y * y * y
  def p(y: Double): Double = {
    count = count + 1
    (3 * y - 4 * cube(y))
  }

  var count2 = 1
  def sin(x: Double): Double = {
    count2 += 1
    if (Math.abs(x) < 0.0001) {
      x
    } else {
      p(sin(x / 3))
    }
  }

  log.info("sin(PI)={} ", sin(12.15))
  log.info("\t count={}\tcount2={}", count, count2)
}