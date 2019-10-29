package tests
import genetics.GeneticAlgorithm._
import genetics.genes._
import genetics.geometry._
import org.scalatest._
class TestLinearRegression extends FunSuite {

  val EPSILON: Double = 0.5

  def equalLine(d1: Line, d2: Line): Boolean = {
    println(d1.toString+" | "+d2.toString)
    (d1.slope - d2.slope).abs <= EPSILON && (d1.yIntercept - d2.yIntercept).abs <= EPSILON
  }



  test("Test 1 横线") {
    val points:Array[Point] = new Array(10)
    val points_x:Array[Double] = Array(5,0,4,9,8,6,3,2,4,8)
    val points_y:Array[Double] = Array(5,5,5,5,5,5,5,5,5,5)
    for(i <- points.indices){
      points(i) = new Point(points_x(i),points_y(i))
    }

    assert(equalLine(linearRegression(points.toList),new Line(0,5)))
  }

  test("Test 2 一次函数") {
    val points:Array[Point] = new Array(2)
    val points_x:Array[Double] = Array(1,5)
    val points_y:Array[Double] = Array(1,5)
    for(i <- points.indices){
      points(i) = new Point(points_x(i),points_y(i))
    }
    assert(equalLine(linearRegression(points.toList),new Line(1,0)))
  }

  test("Test 3 无完美解") {
    val points:Array[Point] = new Array(3)
    val points_x:Array[Double] = Array(2,-7,4)
    val points_y:Array[Double] = Array(-20,-25,20)
    for(i <- points.indices){
      points(i) = new Point(points_x(i),points_y(i))
    }
    assert(equalLine(linearRegression(points.toList),new Line(4.091,3.635)))
  }


  test("Test 4 一次函数") {
    val points:Array[Point] = new Array(5)
    val points_x:Array[Double] = Array(2,3,4,5,6)
    val points_y:Array[Double] = Array(1,5,7,9,13)
    for(i <- points.indices){
      points(i) = new Point(points_x(i),points_y(i))
    }
    assert(equalLine(linearRegression(points.toList),new Line(3,-5)))
  }

  test("Test 5 二次函数") {
    val points:Array[Point] = new Array(5)
    val points_x:Array[Double] = Array(1,2,3,4,5)
    val points_y:Array[Double] = Array(1,4,9,16,25)
    for(i <- points.indices){
      points(i) = new Point(points_x(i),points_y(i))
    }
    assert(equalLine(linearRegression(points.toList),new Line(6,-8)))
  }

  test("Test 6 高中题") {
    val points:Array[Point] = new Array(5)
    val points_x:Array[Double] = Array(2,3,4,5,6)
    val points_y:Array[Double] = Array(2.2,3.8,5.5,6.5,7.0)
    for(i <- points.indices){
      points(i) = new Point(points_x(i),points_y(i))
    }
    assert(equalLine(linearRegression(points.toList),new Line(1.35,-0.25)))
  }

  test("Test 7 随手写的数") {
    val points:Array[Point] = new Array(5)
    val points_x:Array[Double] = Array(3,2,13,6,-3)
    val points_y:Array[Double] = Array(4,9,-8,7,3)
    for(i <- points.indices){
      points(i) = new Point(points_x(i),points_y(i))
    }
    assert(equalLine(linearRegression(points.toList),new Line(-1.2,7.6)))
  }

}
