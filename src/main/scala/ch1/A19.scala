package ch1

import org.slf4j.LoggerFactory
import scala.annotation.tailrec
import scala.util.Random

/**
 * (a,b)
 * a = b*q+a*q+a*p
 * b = b*p+a*q
 * (b*q+a*q+a*p, b*p+a*q)
 *
 * (bp+aq)q+(bq+aq+ap)q + (bq+aq+ap)p = bpq+aqq + bqq + aqq +apq + bqp + aqp + app
 *
 *  b(2qp+qq) + a(2qp+ qq) + a(pp+qq) = 2bqp + bqq + a2qp + aqq + app + aqq
 *
 * bpq+aqq + bqq + aqq +apq + bqp + aqp + app  =app+2aqq + 2aqp  + 2bqp + bqq
 *
 * 2bqp + bqq + a2qp + aqq + app + aqq=app + 2aqq + 2aqp +2bqp + bqq
 *
 *
 * (b*p+a*q)p + (b*q+a*q+a*p)q =  bpp+ aqp + bqq + aqq + apq = b(pp+qq) + a(qp+qq+pq)
 * p'= pp+qq
 * q'=qp+qq+pq
 *
 */
object A19 extends App {
  val log = LoggerFactory.getLogger(A19.getClass())

  def fib(n: Int): Int = {
    
	  @tailrec
    def fibInter(a: Int, b: Int, p: Int, q: Int, count: Int): Int = {
      if (count == 0) {
        b
      } else if (count % 2 == 0) {
        val p1 = p * p + q * q
        val q1 = q * p + q * q + p * q
        fibInter(a, b, p1, q1, count / 2)

      } else {
        val a1 = b * q + a * q + a * p
        val b1 = b * p + a * q
        fibInter(a1, b1, p, q, count - 1)
      }

    }
    fibInter(1, 0, 0, 1, n)
  }
  
  for(i<- 0 to 25){
    log.info("fib({})={}" , i , fib(i))
  }
}