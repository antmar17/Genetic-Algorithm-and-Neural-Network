package genetics
import genetics.Animal

import scala.collection.immutable
import scala.util.Random

object GeneticAlgorithm {

  /**
   * Uses a genetic algorithm to optimize a generic problem
   *
   * @param incubator     Determines how instances of type T are created from a List of Doubles (genes)
   * @param costFunction  Determines the cost for a given instance of T
   * @param numberOfGenes The size of the List expected by the incubator
   * @tparam T The type to be optimized
   * @return An instance of T with minimal cost
   */

  //todo FIRST TRY!
//
//    --------------------------------------------------------------------------------------------
//    Makes list of Double randomly to make Animal
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

    //--------------------------------------------------------------------------------------------
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

    //--------------------------------------------------------------------------------------------

    def createAnimalList[T](index: Int, L: List[Animal], incubator: List[Double] => T, costFunction: T => Double, geneList: List[List[Double]]): List[Animal] = {
      if (geneList.isEmpty) {
        return List()
      }
      else if (L.length < geneList.length) {

        val x = incubator(geneList(index))
        val cost = costFunction(x)
        val animal: Animal = new Animal(geneList(index), cost)
        val y: List[Animal] = L :+ animal
        createAnimalList(index + 1, y, incubator, costFunction, geneList)

      }
      else {
        return L
      }

      //--------------------------------------------------------------------------------------------

    }

    //list must be Empty
//    def mutate(inputList: List[Double], inputAnimal: List[Double], index: Int, indexTotake:Int, randomValue:Double): List[Double] = {
//
//      if (index < inputAnimal.length) {
//        if ( index==indexTotake) {
//
//          val y: List[Double] = inputList :+ randomValue
//
//          return mutate(y, inputAnimal, index + 1, indexTotake,randomValue)
//        }
//
//        else {
//          val o = inputAnimal(index)
//          val z: List[Double] = inputList :+ o
//          return mutate(z, inputAnimal, index + 1, indexTotake,randomValue)
//
//        }
//      }
//      else {
//        return inputList
//      }
//    }
def mutate(inputList: List[Double], inputAnimal: List[Double], index: Int, indexsTotake:List[Int]): List[Double] = {

  if (index < inputAnimal.length) {
    if ( indexsTotake.contains(index)) {
      val random= -100 +200*Random.nextDouble()

      val y: List[Double] = inputList :+ random

      return mutate(y, inputAnimal, index + 1, indexsTotake)
    }

    else {
      val o = inputAnimal(index)
      val z: List[Double] = inputList :+ o
      return mutate(z, inputAnimal, index + 1, indexsTotake)

    }
  }
  else {
    return inputList
  }
}
    //--------------------------------------------------------------------------------------------



    //--------------------------------------------------------------------------------------------

    def animalsToDoubles(inputList: List[List[Double]], animalList: List[Animal]): List[List[Double]] = {
      if (inputList.length < animalList.length) {
        val x = inputList :+ animalList(inputList.length).genes
        animalsToDoubles(x, animalList)
      }
      else {
        return inputList
      }
    }

    //--------------------------------------------------------------------------------------------

    def crossOverTwoAnimal(offSpringList: List[Double], genes1: List[Double], genes2: List[Double], index: Int): List[Double] = {
      if (index < genes1.length) {
        val average: Double = (genes1(index) + genes2(index)) / 2
        val x = offSpringList :+ average
        crossOverTwoAnimal(x, genes1, genes2, index + 1)
      }
      else {
        return offSpringList
      }

    }

val selectRandomIndicies:(Int)=>List[Int]=(inputlength:Int)=>(for(i<-0 until (math.ceil((1.0*inputlength.toDouble)/4.0)).toInt) yield Random.nextInt(inputlength)).toList

  val MSG:(List[Double],List[Int],Int)=>List[List[Double]]=(modelAnimal:List[Double],indexesToSubtract:List[Int],max:Int)=>(for(i<-0 until max)yield {
    mutate(List(), modelAnimal, 0,indexesToSubtract)
  }).toList

  def makesecondGen(modelAnimal:List[Double],inputList:List[List[Double]],max:Int):List[List[Double]]={
    if(inputList.length<max){
      val x=modelAnimal


      val randomIndexSubtract: Int = Random.nextInt(x.length)
      val randomValue: Double = -100 + (200) * Random.nextDouble

      val ret:List[Double]=mutate(List(),x,0,selectRandomIndicies(x.length))
      val y=inputList:+ret
      makesecondGen(modelAnimal,y,max)


    }
    else{
      return inputList
    }
  }

    //--------------------------------------------------------------------------------------------


  //head will be best case
  def geneticHelper[T](maxGenNumber:Int,currentGenNumber:Int,population:List[List[Double]],incubator: List[Double] => T, costFunction: T => Double): List[List[Double]] ={

    if(costFunction(incubator(population.head)) <= 2 || currentGenNumber==maxGenNumber){
      return population

    }
    else{
      val costMap: List[Animal] = createAnimalList(0, List(), incubator, costFunction, population)
      val gen1 = costMap.sortWith(comparator)
      val survivors: List[Animal] = List(gen1.head, gen1(1))

      val backToGenes: List[List[Double]] = animalsToDoubles(List(), survivors)
      val crossover: List[Double] = crossOverTwoAnimal(List(), backToGenes.head, backToGenes(1), 0)

      val combine: List[List[Double]] = backToGenes :+ crossover


//      val secondGen: List[List[Double]] = makesecondGen(backToGenes.head, combine, 8)
      val mutatebrothas=makesecondGen(backToGenes.head,combine,5)
      val secondGen=makePopulation(10, mutatebrothas,backToGenes.head.length)
      geneticHelper(maxGenNumber,currentGenNumber+1,secondGen,incubator,costFunction)
    }

  }

    val comparator: (Animal, Animal) => Boolean = (a1: Animal, a2: Animal) => a1.cost < a2.cost

    def geneticAlgorithm[T](incubator: List[Double] => T, costFunction: T => Double, numberOfGenes: Int): T = {


      val startgenes: List[List[Double]] = makePopulation(10, List(), numberOfGenes)
      val bestcase:List[List[Double]]=geneticHelper(10000,0,startgenes,incubator,costFunction)
      return incubator(bestcase.head)

    }

  def main(args: Array[String]): Unit = {
    val animal1:List[Double]=List(3,2,1)
    val animal2:List[Double]=List(1,2,3)

    val baby=crossOverTwoAnimal(List(),animal1,animal2,0)
    val pop:List[List[Double]]=List(animal1,animal2,baby)

    val second=makesecondGen(baby,pop,8)
    val nextGen=makePopulation(10,second,3)
    println(second)
    println(nextGen)



  }

  //-------------------------------------------------------------------------------------------------------
  //todo SECOND TRY(USING MAP INSTEAD OF ANIMAL OBJECT)<-ABANDONED ANIMAL OBJECT WORKED BETTER

//
//def makeGenes(maxGenes: Int, L: List[Double]): List[Double] = {
//
//  if (L.length < maxGenes) {
//
//    //Next Double returns a random value between 0.0 and 1.0
//    val randomValue: Double = -100 + 200 * Random.nextDouble()
//    val x: List[Double] = L :+ randomValue
//    makeGenes(maxGenes, x)
//  }
//  else {
//
//
//    return L
//  }
//
//}
//
//  /* @param incubator Determines how instances of type T are created from a List of Doubles (genes)*/
//  //--------------------------------------------------------------------------------------------
//  def makePopulation(numAnimals: Int, inputList: List[List[Double]], numberOfGenes: Int): List[List[Double]] = {
//
//    if (inputList.length < numAnimals) {
//      val gene: List[Double] = makeGenes(numberOfGenes, List())
//
//      val x: List[List[Double]] = inputList :+ gene
//      makePopulation(numAnimals, x, numberOfGenes)
//
//    }
//    else {
//
//      return inputList
//    }
//
//  }
//
//  //--------------------------------------------------------------------------------------------
//
//  def createAnimalList[T](index: Int, L: List[Animal], incubator: List[Double] => T, costFunction: T => Double, geneList: List[List[Double]]): List[Animal] = {
//    if (geneList.isEmpty) {
//      return List()
//    }
//    else if (L.length < geneList.length) {
//
//      val x = incubator(geneList(index))
//      val cost = costFunction(x)
//      val animal: Animal = new Animal(geneList(index), cost)
//      val y: List[Animal] = L :+ animal
//      createAnimalList(index + 1, y, incubator, costFunction, geneList)
//
//    }
//    else {
//      return L
//    }
//
//    //--------------------------------------------------------------------------------------------
//
//  }
//
//  //list must be Empty
//  def mutate(inputList: List[Double], inputAnimal: List[Double], index: Int, indexTotake: Int, randomValue:Double): List[Double] = {
//
//    if (index < inputAnimal.length) {
//      if (index == indexTotake) {
//
//        val y: List[Double] = inputList :+ randomValue
//
//        return mutate(y, inputAnimal, index + 1, indexTotake,randomValue)
//      }
//
//      else {
//        val o = inputAnimal(index)
//        val z: List[Double] = inputList :+ o
//        return mutate(z, inputAnimal, index + 1, indexTotake,randomValue)
//
//      }
//    }
//    else {
//      return inputList
//    }
//  }
//
//  //--------------------------------------------------------------------------------------------
//
//  def makeMutatedList(inputList: List[List[Double]], survivors: List[List[Double]], index: Int): List[List[Double]] = {
//    if (survivors.isEmpty) {
//      List()
//    }
//    else if (index < survivors.length) {
//      //MAKES RANDOM INDEX AND VALUE
//      val randomOb = new scala.util.Random()
//
//      val randomIndexSubtract: Int = randomOb.nextInt(survivors.head.length)
//      val randomValue: Double = -100 + (100 - -100) * randomOb.nextDouble
//      //---------------------------------------------------------------
//
//      //MAKE MUTATED ANIMAL AND ADD IT TO THE LIST
//      val mutatedAnimal: List[Double] = mutate(List(), survivors(index),0 ,randomIndexSubtract,randomValue )
//      val x = inputList :+ mutatedAnimal
//
//      //DO IT AGIAN!!!
//      makeMutatedList(x, survivors, index + 1)
//    }
//    else {
//      return inputList
//    }
//  }
//
//  //--------------------------------------------------------------------------------------------
//
//  def animalsToDoubles(inputList: List[List[Double]], animalList: List[Animal]): List[List[Double]] = {
//    if (inputList.length < animalList.length) {
//      val x = inputList :+ animalList(inputList.length).genes
//      animalsToDoubles(x, animalList)
//    }
//    else {
//      return inputList
//    }
//  }
//
//  //--------------------------------------------------------------------------------------------
//
//  def crossOverTwoAnimal(offSpringList: List[Double], genes1: List[Double], genes2: List[Double], index: Int): List[Double] = {
//    if (index < genes1.length) {
//      val average: Double = (genes1(index) + genes2(index)) / 2
//      val x = offSpringList :+ average
//      crossOverTwoAnimal(x, genes1, genes2, index + 1)
//    }
//    else {
//      return offSpringList
//    }
//
//  }
//
//
//  def makesecondGen(modelAnimal:List[Double],inputList:List[List[Double]],max:Int):List[List[Double]]={
//    if(inputList.length<max){
//      val x=modelAnimal
//      val randomOb = new scala.util.Random()
//
//      val randomIndexSubtract: Int = randomOb.nextInt(x.length)
//      val randomValue: Double = -100 + (100 - -100) * randomOb.nextDouble
//
//      val ret:List[Double]=mutate(List(),x,0,randomIndexSubtract,randomValue)
//      val y=inputList:+ret
//      makesecondGen(modelAnimal,y,max)
//
//
//    }
//    else{
//      return inputList
//    }
//  }
//
//  //--------------------------------------------------------------------------------------------
//
//  val comparator: (Animal, Animal) => Boolean = (a1: Animal, a2: Animal) => a1.cost < a2.cost
//  val x=0
//  def startGenCheck(inputNum:Int)
//
//
//  def geneticAlgorithm[T](incubator: List[Double] => T, costFunction: T => Double, numberOfGenes: Int): T = {
//
//
//
//
//
//
//
//
//incubator(null)
//  }
//
//
//  def main(args: Array[String]): Unit = {
//    println(makeGenes(10,List()))
//    println(-100+ 200*Random.nextDouble())
//
//  }




  }