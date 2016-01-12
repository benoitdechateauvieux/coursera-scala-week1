import scala.annotation.tailrec

def sum(f: Int => Int)(a: Int, b: Int): Int = {
  @tailrec
  def loop(a: Int, acc: Int): Int = {
    if (a>b) acc
    else loop(a+1, acc+f(a))
  }
  loop(a, 0)
}

sum(x=>x)(1, 9000)

def sumNoRec(f:Int => Int, a: Int, b: Int): Int = {
  if (a > b) 0
  else f(a) + sumNoRec(f, a + 1, b)
}

sumNoRec(x=>x, 1, 9000)
