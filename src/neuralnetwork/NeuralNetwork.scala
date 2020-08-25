package neuralnetwork
import genetics._
import neuralnetwork.NeuralNetwork

import scala.io.{BufferedSource, Source}
import scala.util.Random
object NeuralNetwork {



def computeCost(index:Int,O_LIST:List[OutputNeuron],correctAnswer:String,counter:Double): Double ={
  if(index==O_LIST.length){

    return counter
  }
  else{
    val neuron=O_LIST(index)
    if(neuron.category==correctAnswer){
      val x=counter+math.pow(neuron.computedOutputValue-1,2)
      computeCost(index+1,O_LIST,correctAnswer,x)

    }
    else{
      val y=counter+math.pow(neuron.computedOutputValue-0,2)
      computeCost(index+1,O_LIST,correctAnswer,y)


    }

  }
}



  //training data(MAP[GENES->FLOWERTYPE])
  def costFunction(trainingData: Map[List[Double], String]): NeuralNetwork => Double = {

    val computedList: (NeuralNetwork) => Double = (n: NeuralNetwork) =>((for (i <- trainingData.toList) yield computeCost(0, n.findOutputs(i._1), i._2, 0))).sum
    computedList

  }

  def makeIncubator(numberOfInputs: Int, outputCategories: List[String]): List[Double] => NeuralNetwork = {
    val numberOfgenes = numberOfDimensions(numberOfInputs, outputCategories.length)
    val retval: (List[Double]) => NeuralNetwork = (genes: List[Double]) => new NeuralNetwork(numberOfInputs, outputCategories, genes)


    retval

  }

  def numberOfDimensions(numberOfInputNodes: Int, numberOfCategories: Int): Int = {
    val numberOfhidden = math.ceil(numberOfCategories + ((2 / 3) * numberOfInputNodes + numberOfCategories)).toInt
    //biases(of hidden neurons)     weights(input->hidden)  weights(hidden->output)         biases(of output neurons)
    numberOfhidden + (numberOfInputNodes * numberOfhidden)+(numberOfhidden*numberOfCategories)+numberOfCategories
  }


}





class NeuralNetwork(numberOfInputNodes: Int, outputCategories: List[String], genes: List[Double]) {

  def makeHiddenNeurons(index: Int, inputList: List[HiddenNeuron], bList: List[Double], wList: List[Double], startingNeuronsList: List[InputNeuron]): List[HiddenNeuron] = {
//    println("")
//    println("this WLIST LENGTH")
//    println(wList.length)
//    println("")


    if (index == bList.length) {
      return inputList
    }
    else {
      val thisWeights = wList.slice(0, numberOfInputNodes)
//      println("New Hidden neuron")
//      println("----------------")
//
//      println("")
//      println("")
//      println("this weights")
//      println(thisWeights)
//      println("")

      val thisbias = bList(index)
//      println("")
//      println("bias")
//      println(thisWeights)
//      println("")
      val remainingList = wList.slice(numberOfInputNodes , wList.length+1)
//      println("")
//      println("remaining List")
//      println(remainingList)
//      println("")
      val node = new HiddenNeuron(thisbias, startingNeuronsList, thisWeights)
      val retval = inputList :+ node
      return makeHiddenNeurons(index + 1, retval, bList, remainingList, startingNeuronsList)
    }
  }
  def makeOutputNeurons(index: Int, inputList: List[OutputNeuron], bList: List[Double], wList: List[Double], startingNeuronsList: List[HiddenNeuron]): List[OutputNeuron] = {

    if (index== bList.length) {

      return inputList
    }

    else {

      val thisWeights = wList.slice(0, numberOfHidden)

      val thisbias = bList(index)
//      println("")
//      println("bias")
//      println(thisWeights)
//      println("")
      val remainingList = wList.slice(outputCategories.length , wList.length+1)
//      println("")
//      println("remaining List")
//      println(remainingList)
//      println("")
      val node = new OutputNeuron(thisbias, startingNeuronsList, thisWeights,outputCategories(index))
      val retval = inputList :+ node
      return makeOutputNeurons(index + 1, retval, bList, remainingList, startingNeuronsList)
    }
  }



//  val numberOfHidden = (math.ceil(5.6)).toInt
  val numberOfHidden = (math.ceil((2.0*numberOfInputNodes.toDouble)/3.0)).toInt+outputCategories.length

  val numberOfWeights_I_H = numberOfInputNodes * numberOfHidden
  val numberOfWeights_H_O = outputCategories.length * numberOfHidden
  val numberofBiases_H=numberOfHidden
  val numberofBiases_O=outputCategories.length


//head is the one that is chose
  def findOutputs(inputs: List[Double]): List[OutputNeuron] = {


    val InputNeuronsList: (List[InputNeuron]) = (for (i <- 0 until inputs.length) yield new InputNeuron(inputs(i))).toList


//    println("InputList FLowergenes")
//    println(InputNeuronsList.map(_.flowergene))
//    println("")
    val BLIST_H:List[Double]=(for(i<-0 until numberofBiases_H)yield genes(i)).toList

//    println("BIAS OF HIDDEN NEURONS")
//    println(BLIST_H)
//    println("")

    val prev1=numberofBiases_H+numberOfWeights_I_H
//    println("PREV1")
//    println(prev1)
//    println("")
    val WLIST_I_to_H:List[Double]=(for (i <- numberofBiases_H until prev1) yield genes(i)).toList

//    println("WLIST_I_to_H")
//    println(WLIST_I_to_H)
//    println("")

    val H_LIST:List[HiddenNeuron]=makeHiddenNeurons(0,List(),BLIST_H,WLIST_I_to_H,InputNeuronsList)
//    println("H_LIST COMPUTATION")
//    println(H_LIST.map(_.computedActivation))
//    println("")


    val prev2=prev1+outputCategories.length
    val BLIST_O:List[Double]=(for(i<-prev1 until prev2) yield genes(i)).toList
//    println("BLIST_O")
//    println(BLIST_O)
//    println("")
    val WLIST_H_to_O:List[Double]=(for (i <- prev2 until genes.length) yield genes(i)).toList
//    println("WLIST_H_to_O")
//    println(WLIST_H_to_O)
//    println("")

    val O_LIST:List[OutputNeuron]=makeOutputNeurons(0,List(),BLIST_O,WLIST_H_to_O,H_LIST)
//    println("O_LIST")
//    println(O_LIST)
//    println("")

    val compareOutputNeurons: (OutputNeuron, OutputNeuron) => Boolean = (a: OutputNeuron, b: OutputNeuron) => a.computedOutputValue > b.computedOutputValue
    val retval = (O_LIST.sortWith(compareOutputNeurons))
//    println("")
//    println("-----------------------------------------------------")
//    println("")
    retval



    }


  def findCategory(inputs: List[Double]): String= {


    findOutputs(inputs).head.category

  }





//  println("TITLE")
//  println()
//  println("")

  //
//  def findCategory(inputs: List[Double]): String = {
//
//    val InputNeuronsList: (List[InputNeuron]) = (for (i <- 0 until inputs.length) yield new InputNeuron(inputs(i))).toList
//    println("INPUTS")
//     print(InputNeuronsList)
//    println("")
//
//    val WeightList: (List[Double]) = (for (i <- 0 until numberOfWeights) yield genes(i)).toList
//
//    println("WEIGHT LIST")
//      println(WeightList)
//    println("")
//
//
//
//
//    val biasList: List[Double] = (for (i <- numberOfWeights until (numberOfWeights + numberOfHidden)) yield genes(i)).toList
//
//    println("BIAS LIST")
//    println(biasList)
//    println("")
//
//    val hiddenNeuronList: List[HiddenNeuron] = makeHiddenNeurons(0, List(), biasList, WeightList, InputNeuronsList)
//
//
//
//    println("HIDDE NEURONS")
//    println("")
//    for(i<-hiddenNeuronList){
//      println("BIAS:  "+i.bias)
//      println(":  "+i.inputWeights)
//
//    }
//    println("")
//    val outPutNeuronsList: List[OutputNeuron] = (for (i <- this.outputCategories) yield new OutputNeuron(hiddenNeuronList, i))
//
//    val compareOutputNeurons: (OutputNeuron, OutputNeuron) => Boolean = (a: OutputNeuron, b: OutputNeuron) => a.computedOutputValue > b.computedOutputValue
//
//    val retval = (outPutNeuronsList.sortWith(compareOutputNeurons)).head
//    retval.category
//  }


}



