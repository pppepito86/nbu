package ex2

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class ArchitectTest extends FunSuite {

  test("length") {
    assert(Architect.max(List()).isEmpty)
    assert(Architect.max(List(3)).contains(3))
    assert(Architect.max(List(1,3,5)).contains(5))
    assert(Architect.max(List(7,3,5)).contains(7))
    assert(Architect.max(List(7,8,5)).contains(8))
  }

  test("triangleTypet") {
    assert(Architect.triangleType(Triangle(3,4,5,6))=="rectangular")
    assert(Architect.triangleType(Triangle(3,3,3,6))=="equilateral")
    assert(Architect.triangleType(Triangle(3,5,3,6))=="isosceles")
    assert(Architect.triangleType(Triangle(3,2,4,6))=="random")
  }


  test("area") {
    assert(Architect.area(Triangle(3,4,5,6))==6)
    assert(Architect.area(Triangle(3,6,7,10))==35)
    assert(Architect.area(Rectangle(10,5))==50)
    assert(Architect.area(Trapezoid(1,7,5))==20)
    assert(Architect.area(Cube())==(-1))
  }

  test("findRectangulars") {
    assert(Architect.findRectangulars(List()) == 0)
    assert(Architect.findRectangulars(List(Triangle(3,4,5,6), Triangle(6,8,10,6))) == 2)
    assert(Architect.findRectangulars(List(Triangle(3,4,5,6), Triangle(3,6,7,10), Triangle(6,8,10,6), Trapezoid(1,7,5))) == 2)
    assert(Architect.findRectangulars(List(Triangle(3,6,7,10), Triangle(6,8,10,6), Trapezoid(1,7,5))) == 1)
    assert(Architect.findRectangulars(List(Triangle(3,6,7,10), Cube(), Trapezoid(1,7,5))) == 0)
  }

}
