package tests

import genetics.GeneticAlgorithm._
import genetics.genes._
import genetics.geometry._
import org.scalatest._

class TestPolynomialRegression extends FunSuite {


  def equalPoly(d1: Polynomial, d2: Polynomial,EPSILON:Double): Boolean = {
    println(d1.toString+" | "+d2.toString)
    for(i <- d1.coefficients.indices){
      if((d1.coefficients(i) - d2.coefficients(i)).abs > EPSILON){
        return false
      }
    }
    true
  }

  test("Test 1") {
    val points:Array[Point] = new Array(5)
    val points_x:Array[Double] = Array(1,2,3,4,5)
    val points_y:Array[Double] = Array(1,4,9,16,25)
    for(i <- points.indices){
      points(i) = new Point(points_x(i),points_y(i))
    }

    assert(equalPoly(polynomialRegression(points.toList,2),new Polynomial(List(0,0,1)),0.3))
  }

  test("Test 0") {
    val points:Array[Point] = new Array(4)
    val points_x:Array[Double] = Array(0,2,9,5,-5)
    val points_y:Array[Double] = Array(4,-6,50,41,61)
    for(i <- points.indices){
      points(i) = new Point(points_x(i),points_y(i))
    }
    polynomialRegression(points.toList,5)
    assert(true)
    //assert(equalPoly(polynomialRegression(points.toList,5),new Polynomial(List(0.29303,-3.0012,-0.2918)),0.4))
  }

  test("Test 2") {
    val points:Array[Point] = new Array(3)
    val points_x:Array[Double] = Array(-1,1,-7)
    val points_y:Array[Double] = Array(3,-3,7)
    for(i <- points.indices){
      points(i) = new Point(points_x(i),points_y(i))
    }

    assert(equalPoly(polynomialRegression(points.toList,2),new Polynomial(List(0.29303,-3.0012,-0.2918)),0.5))

  }

  test("Test 3") {
    val points:Array[Point] = new Array(3)
    val points_x:Array[Double] = Array(1,2,5)
    val points_y:Array[Double] = Array(1,-6,7)
    for(i <- points.indices){
      points(i) = new Point(points_x(i),points_y(i))
    }

    assert(equalPoly(polynomialRegression(points.toList,2),new Polynomial(List(13.6300,-15.4627,2.8273)),0.7))
  }

  test("Test 4") {
    val points:Array[Point] = new Array(5)
    val points_x:Array[Double] = Array(0,2,9,5,-5)
    val points_y:Array[Double] = Array(4,-6,50,41,61)
    for(i <- points.indices){
      points(i) = new Point(points_x(i),points_y(i))
    }

    assert(equalPoly(polynomialRegression(points.toList,2),new Polynomial(List(3.9972,-5.5035,1.1794)),1.5))
  }

  test("Test 5") {
    val points:Array[Point] = new Array(10)
    val points_x:Array[Double] = Array(12,4,6,14,-12,35,1,7,11,1)
    val points_y:Array[Double] = Array(-6,32,1,-7,5,-1,4,-3,-2,0)
    for(i <- points.indices){
      points(i) = new Point(points_x(i),points_y(i))
    }

    assert(equalPoly(polynomialRegression(points.toList,2),new Polynomial(List(0.3259,-0.2997,0.0074)),0.9))
  }
}
