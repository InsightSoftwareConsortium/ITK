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
#ifndef __itkBackPropagationLayer_h
#define __itkBackPropagationLayer_h

#include "itkCompletelyConnectedWeightSet.h"
#include "itkLayerBase.h"
#include "itkObject.h"
#include "itkMacro.h"

namespace itk
{
namespace Statistics
{
template<class TMeasurementVector, class TTargetVector>
class BackPropagationLayer : public LayerBase<TMeasurementVector, TTargetVector>
{
public:
  typedef BackPropagationLayer                         Self;
  typedef LayerBase<TMeasurementVector, TTargetVector> Superclass;
  typedef SmartPointer<Self>                           Pointer;
  typedef SmartPointer<const Self>                     ConstPointer;

  /** Method for creation through the object factory. */
  itkTypeMacro(BackPropagationLayer, LayerBase);
  itkNewMacro(Self);

  typedef typename Superclass::ValueType              ValueType;
  typedef vnl_vector<ValueType>                       NodeVectorType;
  typedef typename Superclass::InternalVectorType     InternalVectorType;
  typedef typename Superclass::OutputVectorType       OutputVectorType;
  typedef typename Superclass::LayerInterfaceType     LayerInterfaceType;
  typedef CompletelyConnectedWeightSet<TMeasurementVector,TTargetVector>
                                                      WeightSetType;

  typedef typename Superclass::WeightSetInterfaceType WeightSetInterfaceType;
  typedef typename Superclass::InputFunctionInterfaceType
                                                      InputFunctionInterfaceType;
  typedef typename Superclass::TransferFunctionInterfaceType
                                                      TransferFunctionInterfaceType;
  //Member Functions
  virtual void SetNumberOfNodes(unsigned int numNodes);
  virtual ValueType GetInputValue(unsigned int i) const;
  virtual void SetInputValue(unsigned int i, ValueType value);
  
  virtual ValueType GetOutputValue(unsigned int) const;
  virtual void SetOutputValue(unsigned int, ValueType);

  virtual ValueType * GetOutputVector();
  void SetOutputVector(TMeasurementVector value);

  virtual void ForwardPropagate();
  virtual void ForwardPropagate(TMeasurementVector input);
  
  virtual void BackwardPropagate();
  virtual void BackwardPropagate(InternalVectorType errors);
  
  virtual void SetOutputErrorValues(TTargetVector);
  virtual ValueType GetOutputErrorValue(unsigned int node_id) const;

  virtual ValueType GetInputErrorValue(unsigned int node_id) const;
  virtual ValueType * GetInputErrorVector();
  virtual void SetInputErrorValue(ValueType, unsigned int node_id);
  
  virtual ValueType Activation(ValueType);
  virtual ValueType DActivation(ValueType);

  /** Set/Get the bias */
  itkSetMacro( Bias, ValueType );
  itkGetConstReferenceMacro( Bias, ValueType );

protected:
  
  BackPropagationLayer();
  virtual ~BackPropagationLayer();
  
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
