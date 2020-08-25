package neuralnetwork

class OutputNeuron (val bias:Double,val hiddenNeurons:List[HiddenNeuron],val inputWeights:List[Double],val category:String){
  val comparator:(Double,Double)=>Boolean=(a:Double,b:Double)=>(a>b)

  def makeMap(index:Int, inputMap:Map[HiddenNeuron,Double],startNeurons:List[HiddenNeuron], inputWeights:List[Double]): Map[HiddenNeuron,Double]={
    if(index==startNeurons.length){
      return inputMap
    }
    else{
      val x=inputMap+(startNeurons(index)->inputWeights(index))
      return makeMap(index+1,x,startNeurons,inputWeights)
    }

  }
  val weightedMap=makeMap(0,Map(),hiddenNeurons,inputWeights)

  def sigmoid(x:Double):Double={
    val xneg= -1*x
    1/(1+math.exp(xneg))
  }



  def computeActivationValue(index:Int,inputMap:Map[HiddenNeuron,Double],inputList:List[HiddenNeuron],counter:Double):Double={
    if(index==inputMap.size){
      return sigmoid(counter+this.bias)
    }
    else{
      val neuron=inputList(index)
      computeActivationValue(index+1,inputMap,inputList,counter+(inputMap(neuron)*neuron.computedActivation))
    }
  }


  val computedOutputValue:Double=computeActivationValue(0,weightedMap,hiddenNeurons,0)
}
