def product(f: Int=>Int)(a: Int, b: Int): Int = {
  if (a>b) 1 else f(a)*product(f)(a+1, b)
}

product(x=>x)(1, 3)

def factorial(a: Int) = product(x=>x)(1, a)

factorial(5)

def generalFunction(f: Int=>Int, limit: Int, operator: (Int, Int)=>Int)(a: Int, b: Int): Int = {
  if (a>b) limit else operator(f(a),generalFunction(f, limit, operator)(a+1, b))
}

def factorialGeneralized(a: Int) = generalFunction(x=>x, 1, (a,b)=>a*b)(1, a)

factorialGeneralized(5)