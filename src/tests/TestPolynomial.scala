package tests

import genetics.geometry.{Point, _}
import genetics.GeneticAlgorithm
import org.scalatest.FunSuite



class TestPolynomial extends FunSuite {

  val EPSILON: Double = 0.1

  def equalDoubles(d1: Double, d2: Double): Boolean = {
    (d1 - d2).abs < EPSILON
  }


  test("Genetic Algorithm Compute Polynomial Regression") {
//
//    val l pList:List[Point]= List()
//
//
//
//
//
//    println(DList)
//    println(pList)
//
//    val computed=GeneticAlgorithm.geneticAlgorithm(Polynomial.incubator,Polynomial.costFunction(pList),2,List())
//    assert(equalDoubles(computed.coefficients(0),poly.coefficients(0)))
//    assert(equalDoubles(computed.coefficients(1),poly.coefficients(1)))

    val poly2=new Polynomial(List(1.5,-2.2, 5))
    val p2List:List[Point]= List(new Point(-1,poly2.evaluate(-1)),new Point(0,poly2.evaluate(0)),new Point(1,poly2.evaluate(1)))


    val computed2=GeneticAlgorithm.geneticAlgorithm(Polynomial.incubator,Polynomial.costFunction(p2List),3)

    assert(equalDoubles(computed2.coefficients(0),poly2.coefficients(0)))
    assert(equalDoubles(computed2.coefficients(1),poly2.coefficients(1)))
    assert(equalDoubles(computed2.coefficients(1),poly2.coefficients(1)))

  }






}
