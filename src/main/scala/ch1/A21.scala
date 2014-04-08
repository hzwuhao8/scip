package ch1

import org.slf4j.LoggerFactory
import scala.annotation.tailrec
import scala.util.Random

object A21 extends App {
  val log = LoggerFactory.getLogger(A21.getClass())

  def expmod(base: Int, exp: Int, m: Int): Int = {
    if (exp == 0) {
      1
    } else if (exp % 2 == 0) {
      val t1 = expmod(base, (exp / 2), m)
      t1 * t1 % m
    } else {
      val t1 = expmod(base, exp - 1, m)
      t1 * base % m
    }
  }

  def fermatTest(n: Int): Boolean = {
    def tryit(a: Int) = expmod(a, n, n) == a
    tryit(Random.nextInt(n - 1) + 1)
  }

  def fastPrime(n: Int, times: Int): Boolean = {
    if (times == 0) {
      true
    } else if (fermatTest(n)) {
      fastPrime(n, times - 1)
    } else {
      false
    }
  }
  def millerRabinTest(n: Int): Boolean = {

    def tryit(a: Int) = expmod(a, n - 1, n) == 1
    def exist(a: Int) = expmod(a, 2, n) != 1
    val a =  Random.nextInt(n - 1) + 1
    exist(a) && tryit(a)
  }
  
  def fastPrime2(n: Int, times: Int): Boolean = {
    if (times == 0) {
      true
    } else if (millerRabinTest(n)) {
      fastPrime2(n, times - 1)
    } else {
      false
    }
  }

  for (i <- 11.to(100, 2)) {
    val r = fastPrime(i, 20)
    val r2 = fastPrime2(i,20)
    if (r) {
      log.debug(" i={},  fastPrime={}", i, r)
     
    }
     if (r2) {
      log.debug(" i={},  fastPrime2={}", i, r2)
     
    }
  }
  
  val carmichaelSeq = Seq(561, 1105, 1729, 2465, 2821, 6601)
  carmichaelSeq.foreach(i => {
    val r = fastPrime(i, 2000)
    val r2 = fastPrime2(i, 2000)
    log.debug(" i={},  fastPrime={}", i, r)
    log.debug(" i={},  fastPrime2={}", i, r2)
  })
  val nSeq = Seq(1000, 1000 * 10, 1000 * 10 * 10)
  for (n <- nSeq) {
    log.info("n={}", n)
    val t = runtime(() => {
      val rSeq = for (i <- 11.to(n, 2)) yield {
        val r = fastPrime(i, 20)
        r
      }
      log.info("rSeq.size={}", rSeq.size)
      log.info("primeSeq.size={}", rSeq.count(x => x))

    })

    log.info("time={}", t)
  }

  def runtime(f: () => Unit): Long = {
    val s = System.currentTimeMillis()
    f()
    val e = System.currentTimeMillis()
    (e - s)
  }
}