package ch1

import org.slf4j.LoggerFactory
import scala.annotation.tailrec
import scala.util.Random

object A29 extends App {
  val log = LoggerFactory.getLogger(A29.getClass())

  @tailrec
  def sum1(term: Double => Double, a: Double, next: Double => Double, b: Double, v: Double): Double = {
    if (a > b) {
      v
    } else {
      sum1(term, next(a), next, b, v + term(a))
    }
  }
  
 
  def sum2(term: Double => Double, a: Double, next: Double => Double, b: Double, v:Double=0.0): Double = {
   @tailrec
    def iter(a: Double , result: Double): Double = {
    if (a > b) {
      result
    } else {
      iter(next(a), result + term(a))
    }
  }
  iter(a,0.0)
 }
 val sum = sum2 _
  {
    def sumIntegers(a: Int, b: Int): Double = sum(x => x, a, x => x + 1, b, 0)
    val a = 1; val b = 10;
    log.info("sumIntegers({},{})={}", a.toString, b.toString, (sumIntegers(a, b)).toString)
  }

  {
    def sumCubes(a: Int, b: Int): Double = sum(x => x * x * x, a, x => x + 1, b, 0)
    val a = 1; val b = 10;
    log.info("sumCubes({},{})={}", a.toString, b.toString, (sumCubes(a, b)).toString)
  }
  {
    def piSum(a: Double, b: Double): Double = sum(x => 1.0 / (x * (x + 2)), a, x => x + 4, b, 0)
    val a = 1; val b = 1000
    log.info("piSum({},{})={}", a.toString, b.toString, (8 * piSum(a, b)).toString)
  }

  {
    def integral(f: Double => Double, a: Double, b: Double, dx: Double): Double = {
      def next(x: Double) = x + dx
      val s = sum(f, (a + dx / 2.0), next, b, 0)
      s * dx
    }
    val (a, b, dx) = (0, 1, 0.01)
    log.info("integralCubes({},{})={}", a.toString, b.toString, (integral(x => x * x * x, a, b, dx)).toString)
    log.info("integralCubes({},{})={}", a.toString, b.toString, (integral(x => x * x * x, a, b, 0.001)).toString)
      log.info("integralSinx({},{})={}", a.toString, b.toString, (integral( x => Math.sin(x), 0,Math.PI, Math.PI/1000)).toString)
    
    log.info("integralSinx({},{})={}", a.toString, b.toString, (integral( x => Math.sin(x), 0,Math.PI, Math.PI/10000)).toString)
    log.info("integralSinx({},{})={}", a.toString, b.toString, (integral( x => Math.sin(x), 0,Math.PI, Math.PI/100000)).toString)
  }

  {

    def xin(f: Double => Double, a: Double, b: Double, n: Int): Double = {
      val dx = (b - a) / ( n)
      log.info("dx={}", dx)
      def next(x: Double) = x + 2*dx
      val s1 = 4 * sum(f, (a + dx), next, (a + ( n - 1) * dx), 0)
      val s2 = 2 * sum(f, a, next, b, 0)
      val s = s1 + s2 - f(a) - f(b)

      s * dx / 3
    }
    val (a, b, dx) = (0, 1, 0.01)
    log.info("xinCubes({},{})={}", a.toString, b.toString, (xin(x => x * x * x, a, b, 100)).toString)
    log.info("xinCubes({},{})={}", a.toString, b.toString, (xin(x => x * x * x, a, b, 1000)).toString)
    
    log.info("xinSinx({},{})={}", a.toString, b.toString, (xin( x => Math.sin(x), 0,Math.PI, 1000)).toString)
    
    log.info("xinSinx({},{})={}", a.toString, b.toString, (xin( x => Math.sin(x), 0,Math.PI, 10000)).toString)
    log.info("xinSinx({},{})={}", a.toString, b.toString, (xin( x => Math.sin(x), 0,Math.PI, 100000)).toString)
  }
  
  {
    
    
  }
}