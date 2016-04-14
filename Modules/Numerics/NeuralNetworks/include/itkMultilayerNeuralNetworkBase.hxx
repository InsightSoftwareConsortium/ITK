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
#ifndef itkMultilayerNeuralNetworkBase_hxx
#define itkMultilayerNeuralNetworkBase_hxx

#include "itkMultilayerNeuralNetworkBase.h"
#include "itkErrorBackPropagationLearningFunctionBase.h"
#include "itkErrorBackPropagationLearningWithMomentum.h"
#include "itkQuickPropLearningRule.h"

namespace itk
{
namespace Statistics
{
template<typename TMeasurementVector, typename TTargetVector,typename TLearningLayer>
MultilayerNeuralNetworkBase<TMeasurementVector,TTargetVector,TLearningLayer>
::MultilayerNeuralNetworkBase()
{
  typedef ErrorBackPropagationLearningWithMomentum<TLearningLayer,TTargetVector> DefaultLearningFunctionType;
  m_LearningFunction = DefaultLearningFunctionType::New();
  m_LearningRate = 0.001;
  //#define __USE_OLD_INTERFACE  Comment out to ensure that new interface works
#ifdef __USE_OLD_INTERFACE
  m_NumOfLayers = 0;
  m_NumOfWeightSets=0;
#endif
}

template<typename TMeasurementVector, typename TTargetVector,typename TLearningLayer>
void
MultilayerNeuralNetworkBase<TMeasurementVector,TTargetVector,TLearningLayer>
::SetLearningRate(ValueType lr)
{
  m_LearningRate=lr;
  this->Modified();
}

template<typename TMeasurementVector, typename TTargetVector,typename TLearningLayer>
void
MultilayerNeuralNetworkBase<TMeasurementVector,TTargetVector,TLearningLayer>
::SetLearningFunction(LearningFunctionInterfaceType* f)
{
  m_LearningFunction=f;
  this->Modified();
}


template<typename TMeasurementVector, typename TTargetVector,typename TLearningLayer>
MultilayerNeuralNetworkBase<TMeasurementVector,TTargetVector,TLearningLayer>
::~MultilayerNeuralNetworkBase()
{
}

template<typename TMeasurementVector, typename TTargetVector,typename TLearningLayer>
void
MultilayerNeuralNetworkBase<TMeasurementVector,TTargetVector,TLearningLayer>
::AddLayer(LayerInterfaceType* layer)
{
  //Automatically set the layer Id based on position in the layer vector.
  layer->SetLayerId(static_cast<const unsigned int>( m_Layers.size()));
  m_Layers.push_back(layer);
//#define __USE_OLD_INTERFACE  Comment out to ensure that new interface works
#ifdef __USE_OLD_INTERFACE
  m_NumOfLayers++;
#endif
}

template<typename TMeasurementVector, typename TTargetVector,typename TLearningLayer>
typename MultilayerNeuralNetworkBase<TMeasurementVector,TTargetVector,TLearningLayer>::LayerInterfaceType*
MultilayerNeuralNetworkBase<TMeasurementVector,TTargetVector,TLearningLayer>
::GetLayer(int layer_id)
{
  return m_Layers[layer_id].GetPointer();
}


template<typename TMeasurementVector, typename TTargetVector,typename TLearningLayer>
const typename MultilayerNeuralNetworkBase<TMeasurementVector,TTargetVector,TLearningLayer>::LayerInterfaceType*
MultilayerNeuralNetworkBase<TMeasurementVector,TTargetVector,TLearningLayer>
::GetLayer(int layer_id) const
{
  return m_Layers[layer_id].GetPointer();
}

template<typename TMeasurementVector, typename TTargetVector,typename TLearningLayer>
typename MultilayerNeuralNetworkBase<TMeasurementVector,TTargetVector,TLearningLayer>::NetworkOutputType
MultilayerNeuralNetworkBase<TMeasurementVector,TTargetVector,TLearningLayer>
::GenerateOutput(TMeasurementVector samplevector)
{
  this->m_Layers[0]->ForwardPropagate(samplevector);
  unsigned int i;
  for (i = 0; i < this->m_Layers.size() && i < this->m_Weights.size(); i++)
    {
    this->m_Weights[i]->ForwardPropagate(
      this->m_Layers[i]->GetOutputVector() );

    this->m_Layers[i + 1]->ForwardPropagate();
    }
  NetworkOutputType temp_output;
  temp_output.SetSize(this->m_Layers[i]->GetNumberOfNodes());
  for(unsigned int k=0; k<temp_output.Size(); k++)
    {
    temp_output[k]=this->m_Layers[i]->GetOutputVector()[k];
    }
  return temp_output;
}

template<typename TMeasurementVector, typename TTargetVector,typename TLearningLayer>
void
MultilayerNeuralNetworkBase<TMeasurementVector,TTargetVector,TLearningLayer>
::BackwardPropagate(NetworkOutputType errors)
{
  size_t i = this->m_Layers.size();
  i--;
  this->m_Layers[i]->BackwardPropagate(errors);
  i--;
  while (i > 0)
    {
    this->m_Layers[i]->BackwardPropagate();
    i--;
    }
}

template<typename TMeasurementVector, typename TTargetVector,typename TLearningLayer>
void
MultilayerNeuralNetworkBase<TMeasurementVector,TTargetVector,TLearningLayer>
::InitializeWeights()
{
  size_t numberOfWeights = this->m_Weights.size();
  for(unsigned int i=0; i<numberOfWeights; i++)
    {
    this->m_Weights[i]->InitializeWeights();
    }
}

template<typename TMeasurementVector, typename TTargetVector,typename TLearningLayer>
void
MultilayerNeuralNetworkBase<TMeasurementVector,TTargetVector,TLearningLayer>
::UpdateWeights(ValueType itkNotUsed(lr))
{
  size_t i = this->m_Layers.size();
  while(i>1)
    {
    i--;
    m_LearningFunction->Learn(this->m_Layers[i],m_LearningRate);
    this->m_Layers[i]->GetModifiableInputWeightSet()->UpdateWeights(m_LearningRate);
    }
}

template<typename TMeasurementVector, typename TTargetVector,typename TLearningLayer>
void
MultilayerNeuralNetworkBase<TMeasurementVector,TTargetVector,TLearningLayer>
::AddWeightSet(typename LayerInterfaceType::WeightSetInterfaceType* weightset)
{
  weightset->SetWeightSetId(static_cast<unsigned int>( m_Weights.size() ) );
  m_Weights.push_back(weightset);
  //#define __USE_OLD_INTERFACE  Comment out to ensure that new interface works
#ifdef __USE_OLD_INTERFACE
  m_NumOfWeightSets++;
#endif
}

#ifdef __USE_OLD_INTERFACE
//Moved definition to header in attempt to fix compiler issues on MS Express 5.0 compiler.
template<typename TMeasurementVector, typename TTargetVector,typename TLearningLayer>
typename MultilayerNeuralNetworkBase<TMeasurementVector,TTargetVector,TLearningLayer>::LayerInterfaceType::WeightSetInterfaceType*
MultilayerNeuralNetworkBase<TMeasurementVector,TTargetVector,TLearningLayer>
::GetWeightSet(unsigned int id)
{
  return m_Weights[id].GetPointer();
}
#endif

#ifdef __USE_OLD_INTERFACE
template<typename TMeasurementVector, typename TTargetVector,typename TLearningLayer>
const typename MultilayerNeuralNetworkBase<TMeasurementVector,TTargetVector,TLearningLayer>::LayerInterfaceType::WeightSetInterfaceType*
MultilayerNeuralNetworkBase<TMeasurementVector,TTargetVector,TLearningLayer>
::GetWeightSet(unsigned int id) const
{
  return m_Weights[id].GetPointer();
}
#endif


template<typename TMeasurementVector, typename TTargetVector,typename TLearningLayer>
void
MultilayerNeuralNetworkBase<TMeasurementVector,TTargetVector,TLearningLayer>
::SetLearningRule(LearningFunctionInterfaceType* l)
{
  m_LearningFunction = l;
  this->Modified();
}

/** Print the object */
template<typename TMeasurementVector, typename TTargetVector,typename TLearningLayer>
void
MultilayerNeuralNetworkBase<TMeasurementVector,TTargetVector,TLearningLayer>
::PrintSelf( std::ostream& os, Indent indent ) const
{
  os << indent << "MultilayerNeuralNetworkBase(" << this << ")" << std::endl;
  Superclass::PrintSelf( os, indent );
  //os << indent << "m_Layers = " << m_Layers << std::endl;
  //os << indent << "m_Weights = " << m_Weights << std::endl;
  if(m_LearningFunction)
    {
    os << indent << "m_LearningFunction = " << m_LearningFunction << std::endl;
    }
  os << indent << "m_LearningRate = " << m_LearningRate << std::endl;
  os << indent << "NumOfLayers = " << m_Layers.size() << std::endl;
  os << indent << "NumOfWeightSets = " << m_Weights.size() << std::endl;
}

} // end namespace Statistics
} // end namespace itk

#endif
