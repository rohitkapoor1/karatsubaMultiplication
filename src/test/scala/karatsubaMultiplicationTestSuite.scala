import org.scalatest.{FunSuite}

class karatsubaMultiplicationTestSuite extends FunSuite {

   test("karatsubaMultiplication.multiply test with 2 digit numbers") {
     assert(karatsubaMultiplication.getKaratsubaResult(12,15) === 180)
   }
  test("karatsubaMultiplication.multiply test with 4 digit numbers") {
    assert(karatsubaMultiplication.getKaratsubaResult(1234,5678) === 7006652)
  }
  test("karatsubaMultiplication.multiply test with one negative number") {
    assert(karatsubaMultiplication.getKaratsubaResult(-12,15) === -180)
  }
  test("karatsubaMultiplication.multiply test with two negative numbers") {
    assert(karatsubaMultiplication.getKaratsubaResult(-12,-15) === 180)
  }


}
