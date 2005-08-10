/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRBFLayer.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkRBFLayerBase_h
#define __itkRBFLayerBase_h

#include "itkLayerBase.h"
#include "itkObject.h"
#include "itkMacro.h"
#include "itkRadialBasisFunctionBase.h"
#include "itkEuclideanDistance.h"

namespace itk
{
namespace Statistics
{

template<class TVector, class TOutput>
class RBFLayer : public LayerBase<TVector, TOutput>
{
public:       
  
  typedef RBFLayer Self;
  typedef LayerBase<TVector, TOutput> Superclass;
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self> ConstPointer;

  /** Method for creation through the object factory. */
  itkTypeMacro(RBFLayer, LayerBase);  
  itkNewMacro(Self) ;

  typedef typename Superclass::ValueType ValueType;
  typedef typename Superclass::ValuePointer ValuePointer;
  typedef vnl_vector<ValueType> NodeVectorType;
  typedef Array<ValueType> NodeArrayType;
  typedef typename Superclass::OutputVectorType OutputVectorType;
  
  typedef RadialBasisFunctionBase<ValueType> RBFType;

  //Distance Metric
  typedef EuclideanDistance<TVector> DistanceMetricType; 
  typedef typename DistanceMetricType::Pointer DistanceMetricPointer;
  //Member Functions
  void SetNumberOfNodes(unsigned int);
  ValueType GetInputValue(unsigned int i);
  void SetInputValue(unsigned int i, ValueType value);

  ValueType GetOutputValue(int);
  void SetOutputValue(int, ValueType);

  ValuePointer GetOutputVector();
  void SetOutputVector(TVector value);

  void ForwardPropagate();
  void ForwardPropagate(TVector);

  void BackwardPropagate();

  void SetOutputErrorValues(TOutput);
  ValueType GetOutputErrorValue(unsigned int);

  
  ValueType GetInputErrorValue(int);
  ValuePointer GetInputErrorVector();
  void SetInputErrorValue(ValueType, int);

  TVector GetCenter(int i);
  void SetCenter(TVector c,int i);

  ValueType GetRadii(int i);
  void SetRadii(ValueType c,int i);


  ValueType Activation(ValueType);
  ValueType DActivation(ValueType);

  void SetBias(ValueType b);

  ValueType GetBias();

  void SetDistanceMetric(DistanceMetricType* f);
  DistanceMetricPointer GetDistanceMetric(){return m_DistanceMetric;}

  itkSetMacro(NumClasses, int);
  itkGetConstReferenceMacro(NumClasses,int);

  void SetRBF(RBFType* f);
  itkGetObjectMacro(RBF, RBFType);

protected:                

   RBFLayer();
  ~RBFLayer();
  
  /** Method to print the object. */
  virtual void PrintSelf( std::ostream& os, Indent indent ) const;

private:

  typename DistanceMetricType::Pointer  m_DistanceMetric;
  NodeVectorType                        m_NodeInputValues;
  NodeVectorType                        m_NodeOutputValues;
  NodeVectorType                        m_InputErrorValues;
  NodeVectorType                        m_OutputErrorValues;
  std::vector<TVector>                  m_Centers;  // ui....uc
  NodeArrayType                         m_Radii;
  int                                   m_NumClasses;
  ValueType                             m_Bias;
  int                                   m_RBF_Dim;
  typename RBFType::Pointer             m_RBF;
};

} // end namespace Statistics
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
  #include "itkRBFLayer.txx"
#endif

#endif
