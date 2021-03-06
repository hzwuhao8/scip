package ch1

import org.slf4j.LoggerFactory
import scala.annotation.tailrec

object A17 extends App {
  val log = LoggerFactory.getLogger(A17.getClass())

  def double(x: Int): Int = x << 1
  def halve(x: Int): Int = x >> 1
  def isEven(x: Int): Boolean = (x & 0x0001) == 0

  log.info("double(12)={}", double(12))
  log.info("halve(12)={}", halve(12))
  log.info("isEven(12)={}", isEven(12))
  log.info("isEven(13)={}", isEven(13))

  var count = 0
   
  def *(a: Int, b: Int): Int = {
    count += 1 
    log.debug("a={},b={}", a, b )
    if (b == 0 || a == 0) {
      0
    } else if (b == 1) {
      a
    } else if (a == 1) {
      b
    } else {
      if(isEven(a)){
        *( halve(a), double(b))
      }else{
        *( halve(a-1), double(b)) + b 
      }
       
    }
  }
  
  val x = 11
  val y = 13
  log.info(" *({},{})={}", x.toString, y.toString, *(x, y).toString)
}