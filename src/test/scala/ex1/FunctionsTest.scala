package ex1

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class FunctionsTest extends FunSuite {

  test("length") {
    assert(Functions.length(List()) == 0)
    assert(Functions.length(List(3, 2, 11)) == 3)
  }

  test("ifelse") {
    assert(Functions.ifelse(cond = true, 12, 15) == 12)
    assert(Functions.ifelse(cond = false, 12, 15) == 15)
  }

  test("balance") {
    assert(Functions.balance("(a)asda(b)(v)".toList))
    assert(Functions.balance("(((a)))".toList))
    assert(Functions.balance("()(()асдасд)".toList))
    assertResult(false)(Functions.balance(")()".toList))
    assertResult(false)(Functions.balance("((д)".toList))
    assertResult(false)(Functions.balance("((das) (d)(".toList))
  }

  test("toUpperCase") {
    assert(Functions.toUpperCase("alabala".toList) == "ALABALA".toList)
    assert(Functions.toUpperCase("alAbaLa".toList) == "ALABALA".toList)
    assert(Functions.toUpperCase("ALABALA".toList) == "ALABALA".toList)
  }

  private def f = (x: Int) => x%2==0

  test("exists") {
    assert(Functions.exists(List(1,2,3), f))
    assert(Functions.exists(List(2,10), f))
    assertResult(false)(Functions.exists(List(), f))
    assertResult(false)(Functions.exists(List(1,3), f))
  }

  test("filter") {
    assert(Functions.filter(List(1,2,3), f) == List(2))
    assert(Functions.filter(List(2,10), f) == List(2, 10))
    assert(Functions.filter(List(), f) == List())
    assert(Functions.filter(List(1,3), f) == List())
  }

  test("forall") {
    assert(Functions.forall(List(), f))
    assert(Functions.forall(List(2,10), f))
    assertResult(false)(Functions.forall(List(1,2,3), f))
    assertResult(false)(Functions.forall(List(1,3), f))
  }

  test("pascal") {
    assert(Functions.pascal(1, 1) == 1)
    assert(Functions.pascal(2, 1) == 1)
    assert(Functions.pascal(2, 2) == 1)
    assert(Functions.pascal(3, 1) == 1)
    assert(Functions.pascal(3, 2) == 2)
    assert(Functions.pascal(3, 3) == 1)
    assert(Functions.pascal(4, 1) == 1)
    assert(Functions.pascal(4, 2) == 3)
    assert(Functions.pascal(4, 3) == 3)
    assert(Functions.pascal(4, 4) == 1)
    assert(Functions.pascal(5, 1) == 1)
    assert(Functions.pascal(5, 2) == 4)
    assert(Functions.pascal(5, 3) == 6)
    assert(Functions.pascal(5, 4) == 4)
    assert(Functions.pascal(5, 5) == 1)
    assert(Functions.pascal(3, 7) == 0)
  }

}
