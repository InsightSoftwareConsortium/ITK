/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkOneHiddenLayerBackPropagationNeuralNetwork.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __OneHiddenLayerBackPropNeuralNetworkBase_txx
#define __OneHiddenLayerBackPropNeuralNetworkBase_txx

#include "itkOneHiddenLayerBackPropagationNeuralNetwork.h"

namespace itk
{
namespace Statistics
{

/** Constructor */
template<class TVector, class TOutput>
OneHiddenLayerBackPropagationNeuralNetwork<TVector,TOutput>
::OneHiddenLayerBackPropagationNeuralNetwork()
{
  typedef IdentityTransferFunction<ValueType> tfType1;
  m_InputTransferFunction = tfType1::New();

  typedef TanSigmoidTransferFunction<ValueType> tfType2;
  m_HiddenTransferFunction = tfType2::New();
 
  typedef TanSigmoidTransferFunction<ValueType> tfType3;
  m_OutputTransferFunction= tfType3::New();

  typedef SumInputFunction<ValueType*, ValueType> InputFcnType;
  m_InputFunction=InputFcnType::New();
  
  m_NumOfInputNodes  = 0;
  m_NumOfHiddenNodes = 0;
  m_NumOfOutputNodes = 0;
  m_HiddenLayerBias  = 1.0;
  m_OutputLayerBias  = 1.0;
}

/** Intialize */
template<class TVector, class TOutput>
void
OneHiddenLayerBackPropagationNeuralNetwork<TVector,TOutput>
::Initialize()
{
  Superclass::SetNumOfLayers(3);
  
  typedef BackPropagationLayer<TVector, TOutput> layertype;
  typedef CompletelyConnectedWeightSet<TVector, TOutput> 
                                                weighttype;

  typename layertype::Pointer inputlayer = layertype::New();
  inputlayer->SetLayerType(1);
  inputlayer->SetNumberOfNodes(m_NumOfInputNodes);

  typename layertype::Pointer hiddenlayer = layertype::New();
  hiddenlayer->SetLayerType(2);
  hiddenlayer->SetNumberOfNodes(m_NumOfHiddenNodes);

  typename layertype::Pointer outputlayer = layertype::New();
  outputlayer->SetLayerType(3);
  outputlayer->SetNumberOfNodes(m_NumOfOutputNodes);

  typename weighttype::Pointer IW =  weighttype::New();
  IW->SetNumberOfInputNodes(m_NumOfInputNodes);
  IW->SetNumberOfOutputNodes(m_NumOfHiddenNodes);
  IW->SetCompleteConnectivity();
  IW->SetBias(m_HiddenLayerBias);
  IW->SetRange(1.0);  //0.5
  IW->Initialize(); 
  
  typename weighttype::Pointer HW =  weighttype::New();
  HW->SetNumberOfInputNodes(m_NumOfHiddenNodes);
  HW->SetNumberOfOutputNodes(m_NumOfOutputNodes);
  HW->SetCompleteConnectivity();
  HW->SetBias(m_OutputLayerBias);
  HW->SetRange(1.0); //0.5
  HW->Initialize(); 
  
  inputlayer->SetOutputWeightSet(IW);
  hiddenlayer->SetInputWeightSet(IW);
  hiddenlayer->SetOutputWeightSet(HW);
  outputlayer->SetInputWeightSet(HW);

  inputlayer->SetTransferFunction(m_InputTransferFunction);
  hiddenlayer->SetTransferFunction(m_HiddenTransferFunction);
  outputlayer->SetTransferFunction(m_OutputTransferFunction);

  hiddenlayer->SetNodeInputFunction(m_InputFunction); 
  outputlayer->SetNodeInputFunction(m_InputFunction); 

  Superclass::AddLayer(inputlayer);
  Superclass::AddLayer(hiddenlayer);
  Superclass::AddLayer(outputlayer);

  Superclass::AddWeightSet(IW);
  Superclass::AddWeightSet(HW);
}


template<class TVector, class TOutput>
void
OneHiddenLayerBackPropagationNeuralNetwork<TVector,TOutput>
::SetInputTransferFunction(TransferFunctionType* f)
{
  m_InputTransferFunction=f;
}

template<class TVector, class TOutput>
void
OneHiddenLayerBackPropagationNeuralNetwork<TVector,TOutput>
::SetHiddenTransferFunction(TransferFunctionType* f)
{
  m_HiddenTransferFunction=f;
}

template<class TVector, class TOutput>
void
OneHiddenLayerBackPropagationNeuralNetwork<TVector,TOutput>
::SetOutputTransferFunction(TransferFunctionType* f)
{
  m_OutputTransferFunction=f;
}

template<class TVector, class TOutput>
void
OneHiddenLayerBackPropagationNeuralNetwork<TVector,TOutput>
::SetInputFunction(InputFunctionType* f)
{
  m_InputFunction=f;
}

/** Generate output */
template<class TVector, class TOutput>
typename OneHiddenLayerBackPropagationNeuralNetwork<TVector, TOutput>::ValueType*
OneHiddenLayerBackPropagationNeuralNetwork<TVector,TOutput>
::GenerateOutput(TVector samplevector)
{
  return Superclass::GenerateOutput(samplevector);
}

/** Print the object */
template<class TVector, class TOutput>
void  
OneHiddenLayerBackPropagationNeuralNetwork<TVector,TOutput>
::PrintSelf( std::ostream& os, Indent indent ) const 
{ 
  os << indent << "OneHiddenLayerBackPropagationNeuralNetwork(" << this << ")" << std::endl; 
  os << indent << "m_NumOfInputNodes = " << m_NumOfInputNodes << std::endl;
  os << indent << "m_NumOfHiddenNodes = " << m_NumOfHiddenNodes << std::endl;
  os << indent << "m_NumOfOutputNodes = " << m_NumOfOutputNodes << std::endl;
  os << indent << "m_HiddenLayerBias = " << m_HiddenLayerBias << std::endl;
  os << indent << "m_OutputLayerBias = " << m_OutputLayerBias << std::endl;
  os << indent << "m_InputFunction = " << m_InputFunction << std::endl;
  os << indent << "m_InputTransferFunction = " << m_InputTransferFunction << std::endl;
  os << indent << "m_HiddenTransferFunction = " << m_HiddenTransferFunction << std::endl;
  os << indent << "m_OutputTransferFunction = " << m_OutputTransferFunction << std::endl;
  Superclass::PrintSelf( os, indent ); 
} 

} // end namespace Statistics
} // end namespace itk

#endif
