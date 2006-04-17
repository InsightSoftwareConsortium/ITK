/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRBFNetwork.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkRBFNetwork_txx
#define __itkRBFNetwork_txx

#include "itkRBFNetwork.h"

namespace itk
{
namespace Statistics
{

template<class TVector, class TOutput>
RBFNetwork<TVector,TOutput>
::RBFNetwork()
{
  typedef IdentityTransferFunction<ValueType> tfType1;
  InputTransferFunction=tfType1::New();

  typedef GaussianRadialBasisFunction<ValueType> tfType2;
  HiddenTransferFunction = tfType2::New();
 
  typedef IdentityTransferFunction<ValueType> tfType3;
  OutputTransferFunction= tfType3::New();

  typedef SumInputFunction<ValueType*, ValueType> InputFcnType;
  InputFunction=InputFcnType::New();
  
  m_HiddenLayerBias = 1.0;
  m_OutputLayerBias = 1.0;
  m_NumOfInputNodes = 0;
  m_NumOfHiddenNodes = 0;
  m_NumOfOutputNodes = 0;
  m_Classes = 0;
}

template<class TVector, class TOutput>
void
RBFNetwork<TVector,TOutput>
::InitializeWeights()
{
   Superclass::InitializeWeights();
   vnl_matrix<ValueType> rbf_weights(m_NumOfHiddenNodes,m_NumOfInputNodes+1);
   rbf_weights.fill(0.0);
   this->m_Weights[0]->SetWeightValues(rbf_weights.data_block());

   std::cout << "Setting rbf weights to zero" << std::endl;
}

template<class TVector, class TOutput>
void
RBFNetwork<TVector,TOutput>
::Initialize()
{
  Superclass::SetNumOfLayers(3);
  
  typedef typename Superclass::LayerType layertype;
  typedef BackPropagationLayer<TVector, TOutput> bplayertype;
  typedef RBFLayer<TVector, TOutput> rbflayertype;

  typedef CompletelyConnectedWeightSet<TVector, TOutput> 
                                                weighttype;

  typename bplayertype::Pointer inputlayer = bplayertype::New();
  inputlayer->SetLayerType(1);
  inputlayer->SetNumberOfNodes(m_NumOfInputNodes);


  typename rbflayertype::Pointer hiddenlayer = rbflayertype::New();
  hiddenlayer->SetRBF_Dim(m_NumOfInputNodes);
  hiddenlayer->SetLayerType(2);
  hiddenlayer->SetNumClasses(m_Classes); 
  hiddenlayer->SetNumberOfNodes(m_NumOfHiddenNodes);
  
  
  typename bplayertype::Pointer outputlayer = bplayertype::New();
  hiddenlayer->SetNumClasses(m_Classes); 
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

  inputlayer->SetTransferFunction(InputTransferFunction);
  hiddenlayer->SetRBF(HiddenTransferFunction);
  outputlayer->SetTransferFunction(OutputTransferFunction);

  hiddenlayer->SetNodeInputFunction(InputFunction); 
  outputlayer->SetNodeInputFunction(InputFunction); 

  Superclass::AddLayer(inputlayer);
  Superclass::AddLayer(hiddenlayer);
  Superclass::AddLayer(outputlayer);

  Superclass::AddWeightSet(IW);
  Superclass::AddWeightSet(HW);
  /*
  TVector temp1;
  TVector temp2;
  temp1[0]=110;
  temp1[1]=250;
  temp1[2]=50;
  hiddenlayer->SetCenter(temp1,0);

  temp2[0]=99;
  temp2[1]=199;
  temp2[2]=300;
  
  hiddenlayer->SetCenter(temp2,1);
  DistanceMetric=DistanceMetricType::New(); 
  double width = DistanceMetric->Evaluate(temp1,temp2);
  
  hiddenlayer->SetRadii(2*width,0);  
  hiddenlayer->SetRadii(2*width,1);  
  */
  for(int j=0; j<m_Centers.size(); j++)
  {
    hiddenlayer->SetCenter(m_Centers[j],j);
    hiddenlayer->SetRadii(m_Radii[j],j);
  }
    
}

template<class TVector, class TOutput>
void
RBFNetwork<TVector,TOutput>
::SetInputTransferFunction(TransferFunctionType* f)
{
  InputTransferFunction=f;
}

template<class TVector, class TOutput>
void
RBFNetwork<TVector,TOutput>
::SetDistanceMetric(DistanceMetricType* f)
{
  DistanceMetric=f;
}

template<class TVector, class TOutput>
void
RBFNetwork<TVector,TOutput>
::SetHiddenTransferFunction(TransferFunctionType* f)
{
  HiddenTransferFunction=f;
}

template<class TVector, class TOutput>
void
RBFNetwork<TVector,TOutput>
::SetOutputTransferFunction(TransferFunctionType* f)
{
  OutputTransferFunction=f;
}

template<class TVector, class TOutput>
void
RBFNetwork<TVector,TOutput>
::SetInputFunction(InputFunctionType* f)
{
  InputFunction=f;
}

template<class TVector, class TOutput>
typename RBFNetwork<TVector, TOutput>::NetworkOutputType
RBFNetwork<TVector,TOutput>
::GenerateOutput(TVector samplevector)
{
  return Superclass::GenerateOutput(samplevector);
}

template<class TVector, class TOutput>
void 
RBFNetwork<TVector,TOutput>
::SetCenter(TVector c)
{
   m_Centers.push_back(c);
} 

template<class TVector, class TOutput>
void 
RBFNetwork<TVector,TOutput>
::SetRadius(ValueType r)
{
   m_Radii.push_back(r);
} 

/** Print the object */
template<class TVector, class TOutput>
void  
RBFNetwork<TVector,TOutput>
::PrintSelf( std::ostream& os, Indent indent ) const 
{ 
  os << indent << "IdentityTransferFunction(" << this << ")" << std::endl; 
  os << indent << "m_NumOfInputNodes = " << m_NumOfInputNodes << std::endl;
  os << indent << "m_NumOfHiddenNodes = " << m_NumOfHiddenNodes << std::endl;
  os << indent << "m_NumOfOutputNodes = " << m_NumOfOutputNodes << std::endl;
  os << indent << "m_Classes = " << m_Classes << std::endl;
  os << indent << "m_HiddenLayerBias = " << m_HiddenLayerBias << std::endl;
  os << indent << "m_OutputLayerBias = " << m_OutputLayerBias << std::endl; 
  Superclass::PrintSelf( os, indent ); 
} 

} // end namespace Statistics
} // end namespace itk


#endif
