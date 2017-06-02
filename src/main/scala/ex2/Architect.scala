package ex2

import scala.annotation.tailrec

sealed trait Shape
case class Triangle(a: Int, b: Int, c: Int, h: Int) extends Shape // h represent the height against the longest side of the triangle
case class Rectangle(a: Int, b: Int) extends Shape
case class Trapezoid(a: Int, b: Int, h: Int) extends Shape
case class Cube() extends Shape

object Architect {

  /*
   *  Finds the max element from given list of integers.
   *  The result is wrapped in an Option instance. The Option has two forms:
   *   - None if no element satisfies the search criteria (for example, in case an empty list is provided)
   *   - Some(x), where x if the searched element. In this case, it can be acquired with the method get. Example:
   *     val o: Option[Int] = Some(6)
   *     val n: Int = o.get
   */
  def max(xs: List[Int]): Option[Int] = xs match {
    case Nil => None
    case x::Nil => Some(x)
    case x::y::ys => max(math.max(x, y)::ys)
  }

  // Determines the type of given triangle: "rectangular", "equilateral", "isosceles", "random"
  def triangleType(t: Triangle): String = t match {
    case Triangle(a, b, c, h) if a*a+b*b+c*c == 2*max(List(a,b,c)).get*max(List(a,b,c)).get => "rectangular"
    case Triangle(a, b, c, h) if a==b && b==c => "equilateral"
    case Triangle(a, b, c, h) if a==b || b==c || a==c => "isosceles"
    case _ => "random"
  }

  /*
   * Calculates the area of the provided shape, by using these formulae:
   *  - Rectangular triangle: a * b / 2, where a and b are cathetus
   *  - Any triangle except rectangular: x * h / 2, where x is the largest side of the triangle and h is the opposite height
   *  - Rectangle: a * b, where a and b are both sides
   *  - Trapezoid: (a + b) * h / 2, where a and b are the parallel sides and h is the height between them
   *  - Cube: always return -1
   *
   *  Hint: for triangles use the max function
   */
  def area(s: Shape): Double = s match {
    case Triangle(a, b, c, h) => if (triangleType(Triangle(a,b,c,h)) == "rectangular") a*b*c/(2*max(List(a,b,c)).get) else max(List(a,b,c)).get*h/2
    case Rectangle(a, b) => a*b
    case Trapezoid(a,  b, h) => (a+b)*h/2
    case Cube() => -1
  }

  /*
   *  Returns the number of rectangular triangles in given list of shapes
   *
   *  Hint: use the triangleType function
   */
  def findRectangulars(shapes: List[Shape]): Int = {
    def iter(shapes: List[Shape], n: Int): Int = shapes match {
      case Nil => n
      case (t: Triangle)::xs if triangleType(t) == "rectangular" => iter(xs, n+1)
      case _::xs => iter(xs, n)
    }

    iter(shapes, 0)
  }
}