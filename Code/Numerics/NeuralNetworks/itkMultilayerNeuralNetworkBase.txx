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

#ifndef __MultiLayerNeuralNetworkBase_txx
#define __MultiLayerNeuralNetworkBase_txx

#include "itkMultilayerNeuralNetworkBase.h"

namespace itk
{
namespace Statistics
{
template<class TVector, class TOutput>
MultilayerNeuralNetworkBase<TVector,TOutput>
::MultilayerNeuralNetworkBase()
{
  typedef ErrorBackPropagationLearningWithMomentum<LayerType, TOutput> DefaultLearningFunctionType;
  m_LearningFunction = DefaultLearningFunctionType::New();
  m_LearningRate = 0.001;
  m_NumOfLayers = 0;
  m_NumOfWeightSets=0;
}

template<class TVector, class TOutput>
void
MultilayerNeuralNetworkBase<TVector,TOutput>
::SetLearningRate(ValueType lr)
{
  m_LearningRate=lr;
  this->Modified();
}
template<class TVector, class TOutput>
void
MultilayerNeuralNetworkBase<TVector,TOutput>
::SetLearningFunction(LearningFunctionType* f)
{
  m_LearningFunction=f;
  this->Modified();
}


template<class TVector, class TOutput>
MultilayerNeuralNetworkBase<TVector,TOutput>
::~MultilayerNeuralNetworkBase()
{
}

template<class TVector, class TOutput>
void
MultilayerNeuralNetworkBase<TVector,TOutput>
::AddLayer(LayerType* layer)
{
  m_Layers.push_back(layer);
}

template<class TVector, class TOutput>
typename MultilayerNeuralNetworkBase<TVector, TOutput>::LayerType*
MultilayerNeuralNetworkBase<TVector,TOutput>
::GetLayer(int layer_id)
{
  return m_Layers[layer_id].GetPointer();
}



template<class TVector, class TOutput>
typename MultilayerNeuralNetworkBase<TVector, TOutput>::NetworkOutputType
MultilayerNeuralNetworkBase<TVector,TOutput>
::GenerateOutput(TVector samplevector)
{
  this->m_Layers[0]->ForwardPropagate(samplevector);
  unsigned int i;
  for (i = 0; i < this->m_Layers.size() && i < this->m_Weights.size(); i++)
    {
    this->m_Weights[i]->ForwardPropagate(this->m_Layers[i]->GetOutputVector());
    this->m_Layers[i + 1]->ForwardPropagate();
    }
  NetworkOutputType temp_output;
  temp_output.SetSize(this->m_Layers[i]->GetNumberOfNodes());
  for(unsigned int k=0; k<temp_output.Size(); k++)
       temp_output[k]=this->m_Layers[i]->GetOutputVector()[k];
//  return this->m_Layers[i]->GetOutputVector();
  return temp_output;
}

template<class TVector, class TOutput>
void
MultilayerNeuralNetworkBase<TVector,TOutput>
::BackwardPropagate(NetworkOutputType errors)
{
  int i = this->m_Layers.size();
  i--;
  this->m_Layers[i]->BackwardPropagate(errors);
  i--;
  while (i > 0)
    {
    this->m_Layers[i]->BackwardPropagate();
    i--;
    }
}

template<class TVector, class TOutput>
void
MultilayerNeuralNetworkBase<TVector,TOutput>
::InitializeWeights()
{
  int num_wts = this->m_Weights.size();
  for(int i=0; i<num_wts; i++)
    {
    this->m_Weights[i]->InitializeWeights();
    }
}

template<class TVector, class TOutput>
void
MultilayerNeuralNetworkBase<TVector,TOutput>
::UpdateWeights(ValueType itkNotUsed(lr))
{
  int i = this->m_Layers.size();
  while(i>1)
    {
    i--;
    m_LearningFunction->Learn(this->m_Layers[i],m_LearningRate);
    this->m_Layers[i]->GetInputWeightSet()->UpdateWeights(m_LearningRate);
    }
}

template<class TVector, class TOutput>
void
MultilayerNeuralNetworkBase<TVector,TOutput>
::AddWeightSet(WeightSetType* weightset)
{
  m_Weights.push_back(weightset);
  m_NumOfWeightSets++;
}

template<class TVector, class TOutput>
typename MultilayerNeuralNetworkBase<TVector, TOutput>::WeightSetType*
MultilayerNeuralNetworkBase<TVector,TOutput>
::GetWeightSet(int id)
{
  return m_Weights[id].GetPointer();
}


template<class TVector, class TOutput>
void
MultilayerNeuralNetworkBase<TVector,TOutput>
::SetLearningRule(LearningFunctionType* l)
{
  m_LearningFunction = l;
  this->Modified();
}

/** Print the object */
template<class TVector, class TOutput>
void  
MultilayerNeuralNetworkBase<TVector,TOutput>
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
  os << indent << "m_NumOfLayers = " << m_NumOfLayers << std::endl;
} 

} // end namespace Statistics
} // end namespace itk

#endif
