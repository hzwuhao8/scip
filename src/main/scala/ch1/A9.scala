package ch1

object A9 extends App {

  def inc(a: Int): Int = a + 1

  def dec(a: Int): Int = a - 1

  def plus1(a: Int, b: Int): Int = {
    println("a=" + a + "\tb=" + b)
    if (a < 0) {

      dec(plus1(inc(a), b))
    } else if (a == 0) {
      b
    } else {

      inc(plus1(dec(a), b))
    }

  }

  plus1(4, 5) == 9
  plus1(-4, 5) == 1


  def plus2(a: Int, b: Int): Int = {
    println("a=" + a + "\tb=" + b)
    if (a == 0) {
      b
    } else if (a < 0) {
      plus2(inc(a), dec(b))
    } else {
      plus2(dec(a), inc(b))
    }
  }
  
  plus2(4, 5) == 9
  plus2(-4, 5) == 1
}