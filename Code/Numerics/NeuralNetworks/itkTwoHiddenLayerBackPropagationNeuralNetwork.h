/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkTwoHiddenLayerBackPropagationNeuralNetwork.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __TwoHiddenLayerBackPropNeuralNetworkBase_h
#define __TwoHiddenLayerBackPropNeuralNetworkBase_h

#include "itkMultilayerNeuralNetworkBase.h"
#include "itkBackPropagationLayer.h"
#include "itkCompletelyConnectedWeightSet.h"
#include "itkSigmoidTransferFunction.h"
#include "itkLogSigmoidTransferFunction.h"
#include "itkTanSigmoidTransferFunction.h"
#include "itkHardLimitTransferFunction.h"
#include "itkSignedHardLimitTransferFunction.h"
#include "itkGaussianTransferFunction.h"
#include "itkIdentityTransferFunction.h"
#include "itkSumInputFunction.h"
#include "itkProductInputFunction.h"

namespace itk
{
namespace Statistics
{

template<class TVector, class TOutput>
class TwoHiddenLayerBackPropagationNeuralNetwork : public MultilayerNeuralNetworkBase<TVector, TOutput>
{
public:

  typedef itk::Statistics::BackPropagationLayer<TVector, TOutput> LayerType;
  typedef itk::Statistics::CompletelyConnectedWeightSet<TVector, TOutput> WeightType;

  typedef TwoHiddenLayerBackPropagationNeuralNetwork Self;
  typedef MultilayerNeuralNetworkBase<TVector, TOutput> Superclass;
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self> ConstPointer;

  /* Method for creation through the object factory. */
  itkTypeMacro(TwoHiddenLayerBackPropagationNeuralNetwork,
               MultilayerNeuralNetworkBase);  
  itkNewMacro(Self) ;

  typedef typename Superclass::ValueType ValueType;

  //Add the layers to the network.
  // 1 input, 1 hidden, 1 output 
  void Initialize();

  itkSetMacro(NumOfInputNodes, int);
  itkGetConstReferenceMacro(NumOfInputNodes, int);

  itkSetMacro(NumOfHiddenNodes1, int);
  itkGetConstReferenceMacro(NumOfHiddenNodes1, int);

  itkSetMacro(NumOfHiddenNodes2, int);
  itkGetConstReferenceMacro(NumOfHiddenNodes2, int);

  itkSetMacro(NumOfOutputNodes, int);
  itkGetConstReferenceMacro(NumOfOutputNodes, int);

  ValueType* GenerateOutput(TVector samplevector);

protected:

  TwoHiddenLayerBackPropagationNeuralNetwork();
  ~TwoHiddenLayerBackPropagationNeuralNetwork() {};
  
  /** Method to print the object. */
  virtual void PrintSelf( std::ostream& os, Indent indent ) const;

private:

  int m_NumOfInputNodes;
  int m_NumOfHiddenNodes1;
  int m_NumOfHiddenNodes2;
  int m_NumOfOutputNodes;
};

} // end namespace Statistics
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
  #include "itkTwoHiddenLayerBackPropagationNeuralNetwork.txx"
#endif

#endif

