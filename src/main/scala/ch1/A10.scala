package ch1

import org.slf4j.LoggerFactory
object A10 extends App {
  val log = LoggerFactory.getLogger(A10.getClass())
  def a(x: Int, y: Int): Int = {
    //log.debug("x={},y={},", x, y)
    (x, y) match {
      case (_, 0) => 0
      case (0, y) => 2 * y
      case (_, 1) => 2
      case (x, y) => a(x - 1, a(x, y - 1))
    }
  }

  val f = a(0, _: Int)
  val g = a(1, _: Int)
  val h = a(2, _: Int)

  log.info(" a(1,10)={}", a(1, 10))
  log.info("a(2,4)={}", a(2, 4))
  log.info("a(3,3)={}", a(3, 3))
  for(x <- 1 to 5){
	  log.info("f({})={}", x ,f(x))
	  
	  log.info("g({})={}", x ,g(x))
	  
	  log.info("h({})={}", x ,h(x))
  }
  
  
}