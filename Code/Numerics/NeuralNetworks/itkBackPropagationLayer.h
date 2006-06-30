/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBackPropagationLayer.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkBackPropagationLayerBase_h
#define __itkBackPropagationLayerBase_h

#include "itkLayerBase.h"
#include "itkObject.h"
#include "itkMacro.h"

namespace itk
{
namespace Statistics
{
template<class TVector, class TOutput>
class BackPropagationLayer : public LayerBase<TVector, TOutput>
{
public:           
  typedef BackPropagationLayer Self;
  typedef LayerBase<TVector, TOutput> Superclass;
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self> ConstPointer;

  /** Method for creation through the object factory. */
  itkTypeMacro(BackPropagationLayer, LayerBase);  
  itkNewMacro(Self);

  typedef typename Superclass::ValueType ValueType;
  typedef typename Superclass::ValuePointer ValuePointer;
  typedef vnl_vector<ValueType> NodeVectorType;
  typedef typename Superclass::InternalVectorType InternalVectorType;
  typedef typename Superclass::OutputVectorType OutputVectorType;

  //Member Functions
  void SetNumberOfNodes(unsigned int);
  ValueType GetInputValue(unsigned int i);
  void SetInputValue(unsigned int i, ValueType value);

  ValueType GetOutputValue(unsigned int);
  void SetOutputValue(unsigned int, ValueType);

  ValuePointer GetOutputVector();
  void SetOutputVector(TVector value);

  void ForwardPropagate();
  void ForwardPropagate(TVector);

  void BackwardPropagate(InternalVectorType e);
  void BackwardPropagate();

  void SetOutputErrorValues(TOutput);
  ValueType GetOutputErrorValue(unsigned int);

  ValueType GetInputErrorValue(unsigned int);
  ValuePointer GetInputErrorVector();
  void SetInputErrorValue(ValueType, unsigned int);

  ValueType Activation(ValueType);
  ValueType DActivation(ValueType);

  /** Set/Get the bias */
  void SetBias(ValueType b);
  ValueType GetBias();

protected:                

  BackPropagationLayer();
  ~BackPropagationLayer();

  /** Method to print the object. */
  virtual void PrintSelf( std::ostream& os, Indent indent ) const;

private:

  NodeVectorType   m_NodeInputValues;
  NodeVectorType   m_NodeOutputValues;
  NodeVectorType   m_InputErrorValues;
  NodeVectorType   m_OutputErrorValues;
  ValueType        m_Bias;
};

} // end namespace Statistics
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
  #include "itkBackPropagationLayer.txx"
#endif

#endif
