/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkOneHiddenLayerBackPropagationNeuralNetwork_hxx
#define itkOneHiddenLayerBackPropagationNeuralNetwork_hxx

#include "itkOneHiddenLayerBackPropagationNeuralNetwork.h"

namespace itk
{
namespace Statistics
{

/** Constructor */
template<typename TMeasurementVector, typename TTargetVector>
OneHiddenLayerBackPropagationNeuralNetwork<TMeasurementVector,TTargetVector>
::OneHiddenLayerBackPropagationNeuralNetwork()
{
  typedef IdentityTransferFunction<ValueType> tfType1;
  m_InputTransferFunction = tfType1::New();

  typedef TanSigmoidTransferFunction<ValueType> tfType2;
  m_FirstHiddenTransferFunction = tfType2::New();

  typedef TanSigmoidTransferFunction<ValueType> tfType3;
  m_OutputTransferFunction= tfType3::New();

  typedef SumInputFunction<ValueType*, ValueType> InputFcnType;
  m_InputFunction=InputFcnType::New();

  m_NumOfInputNodes = 0;
  m_NumOfFirstHiddenNodes = 0;

  m_NumOfOutputNodes = 0;
  m_FirstHiddenLayerBias  = 1.0;

  m_OutputLayerBias  = 1.0;
}

/** Intialize */
template<typename TMeasurementVector, typename TTargetVector>
void
OneHiddenLayerBackPropagationNeuralNetwork<TMeasurementVector,TTargetVector>
::Initialize()
{
  if(m_NumOfInputNodes == 0 )
    {
    itkExceptionMacro("ERROR:  Number of Input Nodes must be greater than 0!");
    }
  if(m_NumOfFirstHiddenNodes == 0 )
    {
    itkExceptionMacro("ERROR:  Number of Hidden Layer 1 Nodes must be greater than 0!");
    }
  if(m_NumOfOutputNodes == 0 )
    {
    itkExceptionMacro("ERROR:  Number of Output Nodes must be greater than 0!");
    }

  //Define weights of Nodes
  typename LearningLayerType::WeightSetType::Pointer InputLayerOutputWeights = LearningLayerType::WeightSetType::New();
  InputLayerOutputWeights->SetNumberOfInputNodes(m_NumOfInputNodes);
  InputLayerOutputWeights->SetNumberOfOutputNodes(m_NumOfFirstHiddenNodes);
  InputLayerOutputWeights->SetCompleteConnectivity();
  InputLayerOutputWeights->SetBias(m_FirstHiddenLayerBias);
  InputLayerOutputWeights->SetRange(1.0);  //0.5
  InputLayerOutputWeights->Initialize();

  typename LearningLayerType::WeightSetType::Pointer HiddenLayer1OutputWeights = LearningLayerType::WeightSetType::New();
  HiddenLayer1OutputWeights->SetNumberOfInputNodes(m_NumOfFirstHiddenNodes);
  HiddenLayer1OutputWeights->SetNumberOfOutputNodes(m_NumOfOutputNodes);
  HiddenLayer1OutputWeights->SetCompleteConnectivity();
  HiddenLayer1OutputWeights->SetBias(m_OutputLayerBias);
  HiddenLayer1OutputWeights->SetRange(1.0); //0.5
  HiddenLayer1OutputWeights->Initialize();

  //Define layers
  typename LearningLayerType::Pointer inputlayer = LearningLayerType::New();
  inputlayer->SetLayerTypeCode(LearningLayerType::INPUTLAYER);
  inputlayer->SetNumberOfNodes(m_NumOfInputNodes);
  inputlayer->SetTransferFunction(m_InputTransferFunction);
  inputlayer->SetNodeInputFunction(m_InputFunction);

  typename LearningLayerType::Pointer hiddenlayer1 = LearningLayerType::New();
  hiddenlayer1->SetLayerTypeCode(LearningLayerType::HIDDENLAYER);
  hiddenlayer1->SetNumberOfNodes(m_NumOfFirstHiddenNodes);
  hiddenlayer1->SetTransferFunction(m_FirstHiddenTransferFunction);
  hiddenlayer1->SetNodeInputFunction(m_InputFunction);

  typename LearningLayerType::Pointer outputlayer = LearningLayerType::New();
  outputlayer->SetLayerTypeCode(LearningLayerType::OUTPUTLAYER);
  outputlayer->SetNumberOfNodes(m_NumOfOutputNodes);
  outputlayer->SetTransferFunction(m_OutputTransferFunction);
  outputlayer->SetNodeInputFunction(m_InputFunction);

  Superclass::AddLayer(inputlayer);
  Superclass::AddLayer(hiddenlayer1);
  Superclass::AddLayer(outputlayer);

  Superclass::AddWeightSet(InputLayerOutputWeights);
  Superclass::AddWeightSet(HiddenLayer1OutputWeights);

  //HACK:  NOTE:  You can not set the WeightSets until after the layers are added to the network because
  //       the LayerId's must have been set prior to the Weights being added to the layers.
  //       The ordering of putting together the networks is crucial.  Layers must be added to network
  //       prior to weights being added to layers.
  inputlayer->SetOutputWeightSet(InputLayerOutputWeights);
  hiddenlayer1->SetInputWeightSet(InputLayerOutputWeights);
  hiddenlayer1->SetOutputWeightSet(HiddenLayer1OutputWeights);
  outputlayer->SetInputWeightSet(HiddenLayer1OutputWeights);
}


template<typename TMeasurementVector, typename TTargetVector>
void
OneHiddenLayerBackPropagationNeuralNetwork<TMeasurementVector,TTargetVector>
::SetInputTransferFunction(TransferFunctionInterfaceType* f)
{
  m_InputTransferFunction=f;
}

template<typename TMeasurementVector, typename TTargetVector>
void
OneHiddenLayerBackPropagationNeuralNetwork<TMeasurementVector,TTargetVector>
::SetFirstHiddenTransferFunction(TransferFunctionInterfaceType* f)
{
  m_FirstHiddenTransferFunction=f;
}

template<typename TMeasurementVector, typename TTargetVector>
void
OneHiddenLayerBackPropagationNeuralNetwork<TMeasurementVector,TTargetVector>
::SetOutputTransferFunction(TransferFunctionInterfaceType* f)
{
  m_OutputTransferFunction=f;
}

template<typename TMeasurementVector, typename TTargetVector>
void
OneHiddenLayerBackPropagationNeuralNetwork<TMeasurementVector,TTargetVector>
::SetInputFunction(InputFunctionInterfaceType* f)
{
  m_InputFunction=f;
}

/** Generate output */
template<typename TMeasurementVector, typename TTargetVector>
typename OneHiddenLayerBackPropagationNeuralNetwork<TMeasurementVector, TTargetVector>::NetworkOutputType
OneHiddenLayerBackPropagationNeuralNetwork<TMeasurementVector,TTargetVector>
::GenerateOutput(TMeasurementVector samplevector)
{
  return Superclass::GenerateOutput(samplevector);
}

/** Print the object */
template<typename TMeasurementVector, typename TTargetVector>
void
OneHiddenLayerBackPropagationNeuralNetwork<TMeasurementVector,TTargetVector>
::PrintSelf( std::ostream& os, Indent indent ) const
{
  os << indent << "OneHiddenLayerBackPropagationNeuralNetwork(" << this << ")" << std::endl;
  os << indent << "m_NumOfInputNodes = " << m_NumOfInputNodes << std::endl;
  os << indent << "m_NumOfFirstHiddenNodes = " << m_NumOfFirstHiddenNodes << std::endl;
  os << indent << "m_NumOfOutputNodes = " << m_NumOfOutputNodes << std::endl;
  os << indent << "m_FirstHiddenLayerBias = " << m_FirstHiddenLayerBias << std::endl;
  os << indent << "m_OutputLayerBias = " << m_OutputLayerBias << std::endl;
  os << indent << "m_InputFunction = " << m_InputFunction << std::endl;
  os << indent << "m_InputTransferFunction = " << m_InputTransferFunction << std::endl;
  os << indent << "m_FirstHiddenTransferFunction = " << m_FirstHiddenTransferFunction << std::endl;
  os << indent << "m_OutputTransferFunction = " << m_OutputTransferFunction << std::endl;
  Superclass::PrintSelf( os, indent );
}

} // end namespace Statistics
} // end namespace itk

#endif
