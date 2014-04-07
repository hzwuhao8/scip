package ch1

import org.slf4j.LoggerFactory
import scala.annotation.tailrec

object A17  extends App {
  val log = LoggerFactory.getLogger(A17.getClass())
  
 
  def *(a:Int , b:Int): Int = {
    if( b==0 || a==0 ){
      0
    }else if( b == 1){
      a
    }else if ( a== 1){
      b
    }else{
      ( a +  *( a , b-1) )
    }
  }
val x = 11 
val y = 13
  log.info(" *({},{})={}" , x.toString,y.toString, *(x,y).toString)
}