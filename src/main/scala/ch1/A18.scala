package ch1

import org.slf4j.LoggerFactory
import scala.annotation.tailrec
import scala.util.Random

object A18 extends App {
  val log = LoggerFactory.getLogger(A18.getClass())

  def double(x: Int): Int = x << 1
  def halve(x: Int): Int = x >> 1
  def isEven(x: Int): Boolean = (x & 0x0001) == 0

  log.info("double(12)={}", double(12))
  log.info("halve(12)={}", halve(12))
  log.info("isEven(12)={}", isEven(12))
  log.info("isEven(13)={}", isEven(13))

  var count = 0
  
   @tailrec
  def *(a: Int, b: Int, v: Int): Int = {
    count += 1 
    log.debug("a={},b={}", a, b )
    if (b == 0 || a == 0) {
      v
    } else if (b == 1) {
      a +v
    } else if (a == 1) {
      b +v
    } else {
      if(isEven(a)){
        *( halve(a), double(b) , v )
      }else{
        *( halve(a-1), double(b), v+b) 
      }
       
    }
  }
  
  val x = 11
  val y = 13
  log.info(" *({},{})={}", x.toString, y.toString, *(x, y,0).toString)
  
  val x1 = Random.nextInt( 10000)
  val y1 = Random.nextInt( 10000)
  val r1 = *(x1, y1,0)
  log.info(" *({},{})={} , {}", x1.toString, y1.toString, r1.toString ,  (r1 == x1*y1).toString)
  log.info("count={}", count)
}