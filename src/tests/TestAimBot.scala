package tests

import genetics.GeneticAlgorithm
import genetics.aimbot.{AimBot, PhysicsVector}
import org.scalatest.FunSuite


class TestAimBot extends FunSuite {


  val EPSILON: Double = 0.05

  def equalDoubles(d1: Double, d2: Double): Boolean = {
    (d1 - d2).abs < EPSILON
  }


  test("Genetic Algorithms Hits a Moving target") {
    val sourceL:PhysicsVector=new PhysicsVector(0,0,0)
    val targetL:PhysicsVector=new PhysicsVector(1,1,1)
    val targetVelocity=new PhysicsVector(1,1,1)



    val computed = GeneticAlgorithm.geneticAlgorithm(AimBot.incubator, AimBot.costFunction(sourceL,targetL,targetVelocity), 3)




  }

}
