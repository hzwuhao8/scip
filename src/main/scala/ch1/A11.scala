package ch1

import org.slf4j.LoggerFactory
import scala.annotation.tailrec
/**
 * Page 27
 */
object A11 extends App {
  val log = LoggerFactory.getLogger(A11.getClass())

  def f(n: Int): Int = {
    if (n < 3) {
      n
    } else {
      f(n - 1) + 2 * f(n - 2) + 3 * f(n - 3)
    }
  }
  for (n <- 1 to 10) {
    log.info("f({})={}", n, f(n))
  }
  
  def g(n: Int): Int = {
    @tailrec
    def ginter(n:Int , a:Int, b:Int ,c:Int):Int = {
      
      if(n<=2){
        a
      }else{
        val t = a+2*b+3*c
        ginter(n-1,t,a,b)
      }
    }
    ginter(n,2,1,0)
  }
  
  for (n <- 1 to 10) {
    log.info("g({})={}", n, g(n))
  }
  
}