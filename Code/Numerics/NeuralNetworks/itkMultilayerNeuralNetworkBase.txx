/*=========================================================================

Program:   Insight Segmentation & Registration Toolkit
Module:    itkMultilayerNeuralNetworkBase.txx
Language:  C++
Date:      $Date$
Version:   $Revision$

Copyright (c) Insight Software Consortium. All rights reserved.
See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

This software is distributed WITHOUT ANY WARRANTY; without even
the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkMultilayerNeuralNetworkBase_txx
#define __itkMultilayerNeuralNetworkBase_txx

#include "itkMultilayerNeuralNetworkBase.h"
#include "itkErrorBackPropagationLearningFunctionBase.h"
#include "itkErrorBackPropagationLearningWithMomentum.h"
#include "itkQuickPropLearningRule.h"

namespace itk
{
namespace Statistics
{
template<class TMeasurementVector, class TTargetVector,class TLearningLayer>
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

template<class TMeasurementVector, class TTargetVector,class TLearningLayer>
void
MultilayerNeuralNetworkBase<TMeasurementVector,TTargetVector,TLearningLayer>
::SetLearningRate(ValueType lr)
{
  m_LearningRate=lr;
  this->Modified();
}

template<class TMeasurementVector, class TTargetVector,class TLearningLayer>
void
MultilayerNeuralNetworkBase<TMeasurementVector,TTargetVector,TLearningLayer>
::SetLearningFunction(LearningFunctionInterfaceType* f)
{
  m_LearningFunction=f;
  this->Modified();
}


template<class TMeasurementVector, class TTargetVector,class TLearningLayer>
MultilayerNeuralNetworkBase<TMeasurementVector,TTargetVector,TLearningLayer>
::~MultilayerNeuralNetworkBase()
{
}

template<class TMeasurementVector, class TTargetVector,class TLearningLayer>
void
MultilayerNeuralNetworkBase<TMeasurementVector,TTargetVector,TLearningLayer>
::AddLayer(LayerInterfaceType* layer)
{
  //Automatically set the layer Id based on position in the layer vector.
  layer->SetLayerId(m_Layers.size());
  m_Layers.push_back(layer);
//#define __USE_OLD_INTERFACE  Comment out to ensure that new interface works
#ifdef __USE_OLD_INTERFACE
  m_NumOfLayers++;
#endif
}

template<class TMeasurementVector, class TTargetVector,class TLearningLayer>
typename MultilayerNeuralNetworkBase<TMeasurementVector,TTargetVector,TLearningLayer>::LayerInterfaceType*
MultilayerNeuralNetworkBase<TMeasurementVector,TTargetVector,TLearningLayer>
::GetLayer(int layer_id)
{
  return m_Layers[layer_id].GetPointer();
}


template<class TMeasurementVector, class TTargetVector,class TLearningLayer>
const typename MultilayerNeuralNetworkBase<TMeasurementVector,TTargetVector,TLearningLayer>::LayerInterfaceType*
MultilayerNeuralNetworkBase<TMeasurementVector,TTargetVector,TLearningLayer>
::GetLayer(int layer_id) const
{
  return m_Layers[layer_id].GetPointer();
}

template<class TMeasurementVector, class TTargetVector,class TLearningLayer>
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

template<class TMeasurementVector, class TTargetVector,class TLearningLayer>
void
MultilayerNeuralNetworkBase<TMeasurementVector,TTargetVector,TLearningLayer>
::BackwardPropagate(NetworkOutputType errors)
{
  unsigned int i = this->m_Layers.size();
  i--;
  this->m_Layers[i]->BackwardPropagate(errors);
  i--;
  while (i > 0)
    {
    this->m_Layers[i]->BackwardPropagate();
    i--;
    }
}

template<class TMeasurementVector, class TTargetVector,class TLearningLayer>
void
MultilayerNeuralNetworkBase<TMeasurementVector,TTargetVector,TLearningLayer>
::InitializeWeights()
{
  unsigned int num_wts = this->m_Weights.size();
  for(unsigned int i=0; i<num_wts; i++)
    {
    this->m_Weights[i]->InitializeWeights();
    }
}

template<class TMeasurementVector, class TTargetVector,class TLearningLayer>
void
MultilayerNeuralNetworkBase<TMeasurementVector,TTargetVector,TLearningLayer>
::UpdateWeights(ValueType itkNotUsed(lr))
{
  unsigned int i = this->m_Layers.size();
  while(i>1)
    {
    i--;
    m_LearningFunction->Learn(this->m_Layers[i],m_LearningRate);
    this->m_Layers[i]->GetInputWeightSet()->UpdateWeights(m_LearningRate);
    }
}

template<class TMeasurementVector, class TTargetVector,class TLearningLayer>
void
MultilayerNeuralNetworkBase<TMeasurementVector,TTargetVector,TLearningLayer>
::AddWeightSet(typename LayerInterfaceType::WeightSetInterfaceType* weightset)
{
  weightset->SetWeightSetId(m_Weights.size());
  m_Weights.push_back(weightset);
  //#define __USE_OLD_INTERFACE  Comment out to ensure that new interface works
#ifdef __USE_OLD_INTERFACE
  m_NumOfWeightSets++;
#endif
}

#ifdef __USE_OLD_INTERFACE
//Moved definition to header in attempt to fix compiler issues on MS Express 5.0 compiler.
template<class TMeasurementVector, class TTargetVector,class TLearningLayer>
typename MultilayerNeuralNetworkBase<TMeasurementVector,TTargetVector,TLearningLayer>::LayerInterfaceType::WeightSetInterfaceType*
MultilayerNeuralNetworkBase<TMeasurementVector,TTargetVector,TLearningLayer>
::GetWeightSet(unsigned int id)
{
  return m_Weights[id].GetPointer();
}
#endif

#ifdef __USE_OLD_INTERFACE
template<class TMeasurementVector, class TTargetVector,class TLearningLayer>
const typename MultilayerNeuralNetworkBase<TMeasurementVector,TTargetVector,TLearningLayer>::LayerInterfaceType::WeightSetInterfaceType*
MultilayerNeuralNetworkBase<TMeasurementVector,TTargetVector,TLearningLayer>
::GetWeightSet(unsigned int id) const
{
  return m_Weights[id].GetPointer();
}
#endif


template<class TMeasurementVector, class TTargetVector,class TLearningLayer>
void
MultilayerNeuralNetworkBase<TMeasurementVector,TTargetVector,TLearningLayer>
::SetLearningRule(LearningFunctionInterfaceType* l)
{
  m_LearningFunction = l;
  this->Modified();
}

/** Print the object */
template<class TMeasurementVector, class TTargetVector,class TLearningLayer>
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
