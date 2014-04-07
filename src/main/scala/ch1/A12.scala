package ch1

import org.slf4j.LoggerFactory
import scala.annotation.tailrec
/**
 * Page 27
 */
object A12 extends App {
  val log = LoggerFactory.getLogger(A12.getClass())

  def pascal(n: Int): List[Int]={
    
    def pascalnter(n:Int , seq: List[Int]): List[Int]={
      log.debug("n={},seq={}", n , seq)
    if(n==1){
      seq
   
    }else{
     val seq2 =  (seq zip seq.tail )map( xy=> xy._1 + xy._2)
     log.debug("seq2={}", seq2)
     val t =   1 :: seq2 ::: List(1)
     log.debug("t={}", t)
      pascalnter(n-1, t )
    }
    }
    
    pascalnter(n , List(1,1))
  }
  for(n <- 1 to 5 ){
    log.info("\n\n")
	  log.info("p({})={}", n , pascal(n))
  }
}