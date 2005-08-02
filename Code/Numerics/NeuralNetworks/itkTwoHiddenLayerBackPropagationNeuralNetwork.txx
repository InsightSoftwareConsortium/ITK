/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkTwoHiddenLayerBackPropagationNeuralNetwork.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __TwoHiddenLayerBackPropNeuralNetworkBase_txx
#define __TwoHiddenLayerBackPropNeuralNetworkBase_txx

#include "itkTwoHiddenLayerBackPropagationNeuralNetwork.h"

namespace itk
{
namespace Statistics
{

template<class TVector, class TOutput>
TwoHiddenLayerBackPropagationNeuralNetwork<TVector,TOutput>
::TwoHiddenLayerBackPropagationNeuralNetwork()
{
  m_NumOfInputNodes = 0;
  m_NumOfHiddenNodes1 = 0;
  m_NumOfHiddenNodes2 = 0;
  m_NumOfOutputNodes = 0;
}

template<class TVector, class TOutput>
void
TwoHiddenLayerBackPropagationNeuralNetwork<TVector,TOutput>
::Initialize()
{
  Superclass::SetNumOfLayers(4);

  typename LayerType::Pointer inputlayer = LayerType::New();
  inputlayer->SetLayerType(1);
  inputlayer->SetNumberOfNodes(m_NumOfInputNodes);

  typename LayerType::Pointer hiddenlayer1 = LayerType::New();
  hiddenlayer1->SetLayerType(2);
  hiddenlayer1->SetNumberOfNodes(m_NumOfHiddenNodes1);

  typename LayerType::Pointer hiddenlayer2 = LayerType::New();
  hiddenlayer2->SetLayerType(2);
  hiddenlayer2->SetNumberOfNodes(m_NumOfHiddenNodes2);

  typename LayerType::Pointer outputlayer = LayerType::New();
  outputlayer->SetLayerType(3);
  outputlayer->SetNumberOfNodes(m_NumOfOutputNodes);

  typename WeightType::Pointer IW = WeightType::New();
  IW->SetNumberOfInputNodes(m_NumOfInputNodes);
  IW->SetNumberOfOutputNodes(m_NumOfHiddenNodes1);
  IW->SetCompleteConnectivity();
  IW->SetBias(1.0);
  IW->Initialize(); 

  typename WeightType::Pointer HW1 = WeightType::New();
  HW1->SetNumberOfInputNodes(m_NumOfHiddenNodes1);
  HW1->SetNumberOfOutputNodes(m_NumOfHiddenNodes2);
  HW1->SetCompleteConnectivity();
  HW1->SetBias(1.0);
  HW1->Initialize(); 

  typename WeightType::Pointer HW2 = WeightType::New();
  HW2->SetNumberOfInputNodes(m_NumOfHiddenNodes2);
  HW2->SetNumberOfOutputNodes(m_NumOfOutputNodes);
  HW2->SetCompleteConnectivity();
  HW2->SetBias(1.0);
  HW2->Initialize(); 

  inputlayer->SetOutputWeightSet(IW);

  hiddenlayer1->SetInputWeightSet(IW);
  hiddenlayer1->SetOutputWeightSet(HW1);

  hiddenlayer2->SetInputWeightSet(HW1);
  hiddenlayer2->SetOutputWeightSet(HW2);

  outputlayer->SetInputWeightSet(HW2);

  typedef IdentityTransferFunction<ValueType> tfType1;
  typename tfType1::Pointer f1 = tfType1::New();
  
  typedef TanSigmoidTransferFunction<ValueType> tfType2;
  typename tfType2::Pointer f2 = tfType2::New();

  typedef LogSigmoidTransferFunction<ValueType> tfType3;
  typename tfType3::Pointer f3 = tfType3::New();

  typedef TanSigmoidTransferFunction<ValueType> tfType4;
  typename tfType4::Pointer f4 = tfType4::New();

  inputlayer->SetTransferFunction(f1);
  hiddenlayer1->SetTransferFunction(f2);
  hiddenlayer2->SetTransferFunction(f3);
  outputlayer->SetTransferFunction(f4);

  typedef Vector<ValueType> tempvectype; 
  typedef SumInputFunction<ValueType*, ValueType> InputFcnType;
  typename InputFcnType::Pointer inputFcn = InputFcnType::New();
  hiddenlayer1->SetNodeInputFunction(inputFcn); 
  hiddenlayer2->SetNodeInputFunction(inputFcn); 
  outputlayer->SetNodeInputFunction(inputFcn); 

  Superclass::AddLayer(inputlayer);
  Superclass::AddLayer(hiddenlayer1);
  Superclass::AddLayer(hiddenlayer2);
  Superclass::AddLayer(outputlayer);

  Superclass::AddWeightSet(IW);
  Superclass::AddWeightSet(HW1);
  Superclass::AddWeightSet(HW2);
}

template<class TVector, class TOutput>
typename TwoHiddenLayerBackPropagationNeuralNetwork<TVector, TOutput>::ValueType*
TwoHiddenLayerBackPropagationNeuralNetwork<TVector,TOutput>
::GenerateOutput(TVector samplevector)
{
  return Superclass::GenerateOutput(samplevector);
}

/** Print the object */
template<class TVector, class TOutput>
void  
TwoHiddenLayerBackPropagationNeuralNetwork<TVector,TOutput>
::PrintSelf( std::ostream& os, Indent indent ) const 
{ 
  os << indent << "TwoHiddenLayerBackPropagationNeuralNetwork(" << this << ")" << std::endl; 
  os << indent << "m_NumOfInputNodes = " << m_NumOfInputNodes << std::endl;
  os << indent << "m_NumOfHiddenNodes1 = " << m_NumOfHiddenNodes1 << std::endl;
  os << indent << "m_NumOfHiddenNodes2 = " << m_NumOfHiddenNodes2 << std::endl;
  os << indent << "m_NumOfOutputNodes = " << m_NumOfOutputNodes << std::endl;
  Superclass::PrintSelf( os, indent ); 
} 

} // end namespace Statistics
} // end namespace itk

#endif
