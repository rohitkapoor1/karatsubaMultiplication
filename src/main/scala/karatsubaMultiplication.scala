import scala.math.{abs, floorDiv, max, pow}
object karatsubaMultiplication extends App {

  if (args.length != 2) {
    throw new IllegalArgumentException("Please provide two numbers for multiplication")
  }

  val x = args{0}.toInt
  val y = args{1}.toInt

  getKaratsubaResult(x,y)

  def getKaratsubaResult(x: Int, y: Int) : Long = {

    var minusSign = 1

    if (x < 0 && y < 0) {
      minusSign = 1
    }
    else if (x < 0 || y < 0) {
      minusSign = -1
    }

    multiply(abs(x),abs(y)) * minusSign
  }

  def multiply(x: Int, y: Int): Long = {

    val firstDigitLen = x.toString.length
    val secDigitLen = y.toString.length

    // base case for recursion
    if (firstDigitLen ==1 || secDigitLen == 1) {
      return x*y
    }
    val maxDigitLength = max(firstDigitLen,secDigitLen)
    val halfDigitLength = floorDiv(maxDigitLength,2)

    /**
     * Karatsuba Multiplication of x and y can be represented as:
     * (pow(10,n//2)a + b) * (pow(10,n//2)c + d) where:
     * a = x // pow(10,n//2)
     * b = x % pow(10,n//2)
     * c = y // pow(10,n//2)
     * d = y % pow(10,n//2)
     *
     * x*y = (pow(10,n//2)a + b) * (pow(10,n//2)c + d)
     *     = pow(10,2*(n//2)) ac + pow(10,n//2) (ad + bc) + bd
     *     = pow(10,2*(n//2)) ac + bd + pow(10, n//2) * ( (a+b)*(c+d) - ac -bd)
     *
     *  ac and bd can be reused to calculate (ad+bc)
     *
     */

    val a = floorDiv(x, pow(10, halfDigitLength).toInt)
    val b = x % (pow(10, halfDigitLength).toInt)

    val c = floorDiv(y, pow(10, halfDigitLength).toInt)
    val d = y % (pow(10, halfDigitLength).toInt)

    // recursive calls
    val ac = multiply(a,c)
    val bd = multiply(b,d)
    val ad_plus_bc = multiply(a+b,c+d) - ac - bd

    val output = (pow(10,2*halfDigitLength)*ac + bd + pow(10,halfDigitLength) * ad_plus_bc).toLong

    output
  }
}