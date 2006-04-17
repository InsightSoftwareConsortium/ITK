/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkNeuralNetworkObject.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __NeuralNetworkObject_h
#define __NeuralNetworkObject_h

#include "itkDataObject.h"
#include "itkLayerBase.h"
#include "itkWeightSetBase.h"
#include "itkLearningFunctionBase.h"

namespace itk
{
namespace Statistics
{
template<class TVector, class TOutput>
class NeuralNetworkObject : public DataObject
{
public:

  typedef NeuralNetworkObject Self;
  typedef DataObject Superclass;
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self> ConstPointer;
  itkTypeMacro(NeuralNetworkObject, DataObject);

  typedef typename TVector::ValueType ValueType;
  typedef TOutput TargetVectorType;
  typedef Array<ValueType> NetworkOutputType;
 
  typedef WeightSetBase<TVector, TOutput> WeightSetType;
  typedef LayerBase<TVector, TOutput> LayerType;
  typedef typename LayerType::Pointer LayerPointer;
  typedef typename WeightSetType::Pointer WeightSetPointer;
  typedef LearningFunctionBase<LayerType, TOutput> LearningFunctionType;
  typedef typename LearningFunctionType::Pointer LearningFunctionPointer;

  //virtual ValueType* GenerateOutput(TVector samplevector) = 0;
  virtual NetworkOutputType GenerateOutput(TVector samplevector)=0;

  //virtual void BackwardPropagate(TOutput errors) = 0;
  virtual void BackwardPropagate(NetworkOutputType errors) = 0;
  virtual void UpdateWeights(ValueType) = 0;

protected:

  NeuralNetworkObject();
  ~NeuralNetworkObject();
  
  /** Method to print the object. */
  virtual void PrintSelf( std::ostream& os, Indent indent ) const;

  ValueType m_LearningRate;
  
};

} // end namespace Statistics
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
  #include "itkNeuralNetworkObject.txx"
#endif

#endif
