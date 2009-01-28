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

#ifndef __itkNeuralNetworkObject_h
#define __itkNeuralNetworkObject_h

#include "itkDataObject.h"
#include "itkLayerBase.h"
#include "itkWeightSetBase.h"
#include "itkLearningFunctionBase.h"

namespace itk
{
namespace Statistics
{
template<class TMeasurementVector, class TTargetVector >
class NeuralNetworkObject : public DataObject
{
public:
  
  typedef NeuralNetworkObject      Self;
  typedef DataObject               Superclass;
  typedef SmartPointer<Self>       Pointer;
  typedef SmartPointer<const Self> ConstPointer;

  itkTypeMacro(NeuralNetworkObject, DataObject);

  typedef TMeasurementVector                        MeasurementVectorType;
  typedef typename MeasurementVectorType::ValueType ValueType;
  typedef Array<ValueType>                          NetworkOutputType;
  typedef TTargetVector                             TargetVectorType;
  
  typedef LayerBase<TMeasurementVector, TTargetVector> LayerInterfaceType;

  virtual NetworkOutputType GenerateOutput(TMeasurementVector samplevector)=0;

  virtual void BackwardPropagate(NetworkOutputType errors) = 0;
  virtual void UpdateWeights(ValueType) = 0;
  
protected:

  NeuralNetworkObject();
  virtual ~NeuralNetworkObject();

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
