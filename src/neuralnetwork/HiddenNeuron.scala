package neuralnetwork

class HiddenNeuron (val bias:Double,val startNeurons:List[InputNeuron],val inputWeights:List[Double]){

def makeMap(index:Int, inputMap:Map[InputNeuron,Double],startNeurons:List[InputNeuron], inputWeights:List[Double]): Map[InputNeuron,Double]={
  if(index==startNeurons.length){
    return inputMap
  }
  else{
    val x=inputMap+(startNeurons(index)->inputWeights(index))
    return makeMap(index+1,x,startNeurons,inputWeights)
  }

}


  val weightedMap=makeMap(0,Map(),startNeurons,inputWeights)

  def sigmoid(x:Double):Double={
    val xneg= -1*x
    1/(1+math.exp(xneg))
  }

  def computeActivationValue(index:Int,inputMap:Map[InputNeuron,Double],inputList:List[InputNeuron],counter:Double):Double={
    if(index==inputMap.size){
      return sigmoid(counter+this.bias)
    }
    else{
      val neuron=inputList(index)
      computeActivationValue(index+1,inputMap,inputList,counter+(inputMap(neuron)*neuron.flowergene))
    }
}

  val computedActivation:Double=computeActivationValue(0,weightedMap,this.startNeurons,0)


}
