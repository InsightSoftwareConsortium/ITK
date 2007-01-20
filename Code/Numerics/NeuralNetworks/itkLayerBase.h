/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkLayerBase.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkLayerBase_h
#define __itkLayerBase_h

#include <iostream>
#include "itkLightProcessObject.h"
#include "itkWeightSetBase.h"
#include "itkArray.h"
#include "itkVector.h"
#include "itkTransferFunctionBase.h"
#include "itkInputFunctionBase.h"

#include "itkMacro.h"

namespace itk
{
namespace Statistics
{

template<class TVector, class TOutput>
class LayerBase : public LightProcessObject
{

public:
  typedef LayerBase Self;
  typedef LightProcessObject Superclass;
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self> ConstPointer;
  
  /** Method for creation through the object factory. */
  itkTypeMacro(LayerBase, LightProcessObject);

  typedef TVector InputVectorType;
  typedef TOutput OutputVectorType;

  typedef typename TVector::ValueType ValueType;
  typedef ValueType* ValuePointer;
  typedef const ValueType* ValueConstPointer;
  typedef vnl_vector<ValueType> NodeVectorType;
  typedef Array<ValueType> InternalVectorType;

  typedef WeightSetBase<TVector,TOutput> WeightSetType;

  typedef TransferFunctionBase<ValueType> TransferFunctionType;

  typedef InputFunctionBase<ValueType*, ValueType> InputFunctionType;

  typedef typename InputFunctionType::Pointer InputFunctionPointer;
  typedef typename InputFunctionType::ConstPointer InputFunctionConstPointer;

  typedef typename TransferFunctionType::Pointer TransferFunctionPointer;
  typedef typename TransferFunctionType::ConstPointer TransferFunctionConstPointer;

  typedef typename WeightSetType::Pointer WeightSetPointer;
  typedef typename WeightSetType::ConstPointer WeightSetConstPointer;

  virtual void SetNumberOfNodes(unsigned int);
  unsigned int GetNumberOfNodes() const;

  virtual ValueType GetInputValue(unsigned int) const = 0;
  virtual ValueType GetOutputValue(unsigned int) const = 0;
  virtual ValuePointer GetOutputVector() = 0;

  virtual void ForwardPropagate(){};

  virtual void ForwardPropagate(TVector){};

  virtual void BackwardPropagate(InternalVectorType){};

  virtual void BackwardPropagate(){};
  virtual ValueType GetOutputErrorValue(unsigned int) const = 0;
  virtual void SetOutputErrorValues(TOutput) {};

  virtual ValueType GetInputErrorValue(unsigned int) const = 0;
  virtual ValuePointer GetInputErrorVector() = 0;
  virtual void SetInputErrorValue(ValueType, unsigned int) {};

  //itkSetObjectMacro(InputWeightSet, WeightSetType);
  void SetInputWeightSet(WeightSetType*);
  itkGetObjectMacro(InputWeightSet, WeightSetType);
  itkGetConstObjectMacro(InputWeightSet, WeightSetType);

  //itkSetObjectMacro(OutputWeightSet, WeightSetType);
  void SetOutputWeightSet(WeightSetType*);
  itkGetObjectMacro(OutputWeightSet, WeightSetType);
  itkGetConstObjectMacro(OutputWeightSet, WeightSetType);

  void SetNodeInputFunction(InputFunctionType* f);
  itkGetObjectMacro(NodeInputFunction, InputFunctionType);
  itkGetConstObjectMacro(NodeInputFunction, InputFunctionType);

  void SetTransferFunction(TransferFunctionType* f);
  itkGetObjectMacro(ActivationFunction, TransferFunctionType);
  itkGetConstObjectMacro(ActivationFunction, TransferFunctionType);

  virtual ValueType Activation(ValueType) = 0;
  virtual ValueType DActivation(ValueType) = 0;

  itkSetMacro(LayerType, unsigned int);
  itkGetConstReferenceMacro(LayerType, unsigned int);

  itkSetMacro(LayerId,unsigned int);
  itkGetConstReferenceMacro(LayerId,unsigned int);

  virtual void SetBias(ValueType) = 0;
  virtual const ValueType & GetBias() const = 0;

  
protected:

  LayerBase(); 
  ~LayerBase();
  
  /** Method to print the object. */
  virtual void PrintSelf( std::ostream& os, Indent indent ) const;

  unsigned int m_LayerType; //input, hidden, output
  unsigned int m_LayerId; //input, hidden, output
  unsigned int m_NumberOfNodes; 

  typename WeightSetType::Pointer m_InputWeightSet;
  typename WeightSetType::Pointer m_OutputWeightSet;

  TransferFunctionPointer m_ActivationFunction;
  InputFunctionPointer    m_NodeInputFunction;
  
 

}; //class layer base

} //namespace itk
} //namespace statistics

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkLayerBase.txx"
#endif

#endif
