/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRBFNetwork.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkRBFNetwork_h
#define __itkRBFNetwork_h


#include "itkMultilayerNeuralNetworkBase.h"
#include "itkBackPropagationLayer.h"
#include "itkRBFLayer.h"
#include "itkCompletelyConnectedWeightSet.h"
#include "itkSigmoidTransferFunction.h"
#include "itkLogSigmoidTransferFunction.h"
#include "itkSymmetricSigmoidTransferFunction.h"
#include "itkTanSigmoidTransferFunction.h"
#include "itkHardLimitTransferFunction.h"
#include "itkSignedHardLimitTransferFunction.h"
#include "itkGaussianTransferFunction.h"
#include "itkTanHTransferFunction.h"
#include "itkIdentityTransferFunction.h"
#include "itkSumInputFunction.h"

namespace itk
{
namespace Statistics
{
template<class TVector, class TOutput>
class RBFNetwork : public MultilayerNeuralNetworkBase<TVector, TOutput>
{
public:

  typedef RBFNetwork Self;
  typedef MultilayerNeuralNetworkBase<TVector, TOutput> Superclass;
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self> ConstPointer;
  typedef typename Superclass::ValueType ValueType;
  typedef Array<ValueType> ArrayType;
  typedef TransferFunctionBase<ValueType> TransferFunctionType;
  typedef RadialBasisFunctionBase<ValueType> RBFType;
  typedef InputFunctionBase<ValueType*, ValueType> InputFunctionType;
  typedef EuclideanDistance<ArrayType> DistanceMetricType; 

  typename InputFunctionType::Pointer InputFunction;
  typename DistanceMetricType::Pointer DistanceMetric;
  
  typename TransferFunctionType::Pointer InputTransferFunction;
  typename RBFType::Pointer HiddenTransferFunction;
  typename TransferFunctionType::Pointer OutputTransferFunction;
  
  typedef typename Superclass::NetworkOutputType NetworkOutputType;

  /* Method for creation through the object factory. */
  itkTypeMacro(RBFNetwork,
               MultilayerNeuralNetworkBase);  
  itkNewMacro(Self) ;

  //Add the layers to the network.
  // 1 input, 1 hidden, 1 output 
  void Initialize();

  itkSetMacro(NumOfInputNodes, int);
  itkGetConstReferenceMacro(NumOfInputNodes, int);

  itkSetMacro(NumOfHiddenNodes, int);
  itkGetConstReferenceMacro(NumOfHiddenNodes, int);

  itkSetMacro(NumOfOutputNodes, int);
  itkGetConstReferenceMacro(NumOfOutputNodes, int);

  itkSetMacro(HiddenLayerBias, ValueType);
  itkGetConstReferenceMacro(HiddenLayerBias, ValueType);

  itkSetMacro(OutputLayerBias, ValueType);
  itkGetConstReferenceMacro(OutputLayerBias, ValueType);

  itkSetMacro(Classes, int);
  itkGetConstReferenceMacro(Classes,int);

 // ValueType* GenerateOutput(TVector samplevector);
  virtual NetworkOutputType GenerateOutput(TVector samplevector);

  void SetInputTransferFunction(TransferFunctionType* f);
  void SetDistanceMetric(DistanceMetricType* f);
  void SetHiddenTransferFunction(TransferFunctionType* f);
  void SetOutputTransferFunction(TransferFunctionType* f);

  void SetInputFunction(InputFunctionType* f);
  void InitializeWeights();

  void SetCenter(TVector c);
  void SetRadius(ValueType r);

protected:

  RBFNetwork();
  ~RBFNetwork(){};
  
  /** Method to print the object. */
  virtual void PrintSelf( std::ostream& os, Indent indent ) const;

private:

  int       m_NumOfInputNodes;
  int       m_NumOfHiddenNodes;
  int       m_NumOfOutputNodes;
  int       m_Classes;
  ValueType m_HiddenLayerBias;
  ValueType m_OutputLayerBias;
  std::vector<TVector> m_Centers;  // ui....uc
  std::vector<double> m_Radii;
};

} // end namespace Statistics
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkRBFNetwork.txx"
#endif

#endif
