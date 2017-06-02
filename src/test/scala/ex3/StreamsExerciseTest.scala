package ex3

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class StreamsExerciseTest extends FunSuite {

  test("capOfDigitsSizeTask1") {
    assert(StreamsExercise.capOfDigitsSizeTask1 == 5)
  }

  test("resultStreamTask1") {
    assert(StreamsExercise.resultStreamTask1.toList == List(1, 153, 370, 371, 407))
  }

}
