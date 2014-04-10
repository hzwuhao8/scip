package ch1

import org.slf4j.LoggerFactory
import scala.annotation.tailrec
import scala.util.Random

object A29 extends App {
  val log = LoggerFactory.getLogger(A29.getClass())
  
@tailrec
  def sum(term: Double => Double ,a : Double , next: Double=> Double, b: Double, v: Double ): Double = {
    if( a> b){
      v
    }else{
      sum( term , next(a),next , b ,v  +  term(a) )
    }
  }

 
  
  {
    def sumIntegers(a: Int , b: Int): Double = sum(  x=> x , a , x=> x+1 ,b , 0)
    val a =1 ; val  b =10 ;
    log.info("sumIntegers({},{})={}" , a.toString ,b.toString ,(sumIntegers(a,b)).toString  )
  }
  
  {
  def sumCubes(a:Int , b:Int ): Double = sum( x=> x*x*x , a , x => x+1 , b, 0 ) 
  val a =1 ; val  b =10 ;
  log.info("sumCubes({},{})={}" , a.toString ,b.toString ,(sumCubes(a,b)).toString  )
  }
  {
  def piSum(a: Double , b:Double): Double = sum( x=> 1.0/(x*(x+2)) , a, x=>x+4 , b , 0 )
  val a = 1 ; val b = 1000
  log.info("piSum({},{})={}" , a.toString ,b.toString ,   (8 * piSum(a,b)).toString  )
  }
  
}