package genetics

import genetics.GeneticAlgorithm.{makeGenes, makePopulation}

import scala.util.Random

class Generation (animalList:List[Double],genNumber:Int){
  def makeGenes(maxGenes: Int, L: List[Double], maxNum: Double, minNum: Double): List[Double] = {

    if (L.length < maxGenes) {

      //Next Double returns a random value between 0.0 and 1.0
      val randomValue: Double = -100 + 200 * Random.nextDouble()
      val x: List[Double] = L :+ randomValue
      makeGenes(maxGenes, x, maxNum, minNum)
    }
    else {


      return L
    }

  }


  def makePopulation(numAnimals: Int, inputList: List[List[Double]], numberOfGenes: Int): List[List[Double]] = {

    if (inputList.length < numAnimals) {
      val gene: List[Double] = makeGenes(numberOfGenes, List(), 100, -100)

      val x: List[List[Double]] = inputList :+ gene
      makePopulation(numAnimals, x, numberOfGenes)

    }
    else {

      return inputList
    }

  }


}
