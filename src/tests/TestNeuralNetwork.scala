package tests

import genetics.GeneticAlgorithm
import org.scalatest.FunSuite
import neuralnetwork.NeuralNetwork

import scala.io.{BufferedSource, Source}

class TestNeuralNetwork extends FunSuite{
test("tests Neural network"){
  val filename="data\\iris_train.csv"
  val file: BufferedSource =Source.fromFile(filename)
  var map:Map[List[Double],String]=Map()
  for(line<-file.getLines()){
    var x:List[String]=line.split(",").toList
    map+=(List(x(0).toDouble,x(1).toDouble,x(2).toDouble,x(3).toDouble)->x(4))
  }
  val outputCategories=List("versicolor","setosa","virginica")
println("****TRAINING NEURAL NETWORK ONE MOMENT PLEASE!(Estimated time is around 1 min)*****")
 val NN:NeuralNetwork= GeneticAlgorithm.geneticAlgorithm(NeuralNetwork.makeIncubator(4,outputCategories),NeuralNetwork.costFunction(map),NeuralNetwork.numberOfDimensions(4,3))


  def TESTCSV(): Int ={
    var counter=0
    val filenamex="data\\iris_test.csv"
    val file: BufferedSource =Source.fromFile(filenamex)
    var map2:Map[List[Double],String]=Map()
    for(line<-file.getLines()){
      var x:List[String]=line.split(",").toList
      map2+=(List(x(0).toDouble,x(1).toDouble,x(2).toDouble,x(3).toDouble)->x(4))
    }

    for(i<-map2){
      val nnguess=NN.findCategory(i._1)
      println("NN GUESS: "+nnguess+" ACTUAL:"+i._2)
      if(nnguess==i._2){
        counter+=1
      }
    }
    println("")
    println("--------------------------------")
    println("FINAL RESULT: "+counter+" GUESSES CORRCT")
    println("THAT IS "+counter+"/"+map2.size+" CORRECT")
    println(counter.toDouble/map2.size.toDouble+" Correctness rate")
    println("--------------------------------")

    return counter

  }
  TESTCSV()


}
}
