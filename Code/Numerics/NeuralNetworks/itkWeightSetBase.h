/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkWeightSetBase.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkWeightSetBase_h
#define __itkWeightSetBase_h

#include "itkLayerBase.h"
#include "itkLightProcessObject.h"
#include <vnl/vnl_matrix.h>
#include <vnl/vnl_diag_matrix.h>
#include "itkMacro.h"
#include "itkVector.h"
#include "itkMersenneTwisterRandomVariateGenerator.h"
#include <math.h>
#include <stdlib.h>

namespace itk
{
namespace Statistics
{

template<class TVector, class TOutput>
class WeightSetBase : public LightProcessObject
{
public:
  
  typedef WeightSetBase Self;
  typedef LightProcessObject Superclass;
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self> ConstPointer;
  itkTypeMacro(WeightSetBase, LightProcessObject);        

  typedef MersenneTwisterRandomVariateGenerator  RandomVariateGeneratorType;

  typedef typename TVector::ValueType ValueType;
  typedef ValueType* ValuePointer;

  void Initialize();

  ValueType RandomWeightValue(ValueType low, ValueType high);

  void ForwardPropagate(ValuePointer inputlayeroutputvalues);

  void BackwardPropagate(ValuePointer inputerror);

  void SetConnectivityMatrix(vnl_matrix < int>);

  void SetNumberOfInputNodes(int n);
  int GetNumberOfInputNodes();

  void SetNumberOfOutputNodes(int n);
  int GetNumberOfOutputNodes();

  void SetRange(ValueType Range);
 
  ValuePointer GetOutputValues();

  ValuePointer GetInputValues();
  
  ValuePointer GetTotalDeltaValues();
  
  ValuePointer GetTotalDeltaBValues();
  
  ValuePointer GetDeltaValues();

  void SetDeltaValues(ValuePointer);
  
  void SetDWValues(ValuePointer);

  void SetDBValues(ValuePointer);

  ValuePointer GetDeltaBValues();
  
  void SetDeltaBValues(ValuePointer);

  ValuePointer GetDWValues();

  ValuePointer GetPrevDWValues();

  ValuePointer GetPrevDBValues();

  ValuePointer GetPrev_m_2DWValues();

  ValuePointer GetPrevDeltaValues();

  ValuePointer GetPrev_m_2DeltaValues();

  ValuePointer GetPrevDeltaBValues();

  ValuePointer GetWeightValues();
  
  void SetWeightValues(ValuePointer weights);

  void UpdateWeights(ValueType LearningRate);

  void SetMomentum(ValueType);

  ValueType GetMomentum();

  void SetBias(ValueType);

  ValueType GetBias();

  bool GetFirstPass();

  void SetFirstPass(bool);

  bool GetSecondPass();

  void SetSecondPass(bool);

  void InitializeWeights();

protected: 
  
  WeightSetBase();        
  ~WeightSetBase();

  /** Method to print the object. */
  virtual void PrintSelf( std::ostream& os, Indent indent ) const;

  typename RandomVariateGeneratorType::Pointer m_RandomGenerator;
  int                       m_NumberOfInputNodes;
  int                       m_NumberOfOutputNodes;
  vnl_matrix<ValueType>     m_OutputValues;
  vnl_matrix<ValueType>     m_InputErrorValues;
  
  // weight updates dw=lr * del *y
  // DW= current
  // DW_m_1 = previous
  // DW_m_2= second to last
  // same applies for delta and bias values

  vnl_matrix<ValueType> m_DW;            // delta valies for weight update
  vnl_matrix<ValueType> m_DW_new;        // delta valies for weight update
  vnl_matrix<ValueType> m_DW_m_1;        // delta valies for weight update
  vnl_matrix<ValueType> m_DW_m_2;        // delta valies for weight update
  vnl_matrix<ValueType> m_DW_m;          // delta valies for weight update
  
  vnl_vector<ValueType> m_DB;            // delta values for bias update
  vnl_vector<ValueType> m_DB_new;        // delta values for bias update
  vnl_vector<ValueType> m_DB_m_1;        // delta values for bias update
  vnl_vector<ValueType> m_DB_m_2;        // delta values for bias update
  
  vnl_matrix<ValueType> m_del;           // dw=lr * del * y
  vnl_matrix<ValueType> m_del_new;       // dw=lr * del * y
  vnl_matrix<ValueType> m_del_m_1;       // dw=lr * del * y
  vnl_matrix<ValueType> m_del_m_2;       // dw=lr * del * y
  
  vnl_vector<ValueType> m_delb;          // delta values for bias update
  vnl_vector<ValueType> m_delb_new;      // delta values for bias update
  vnl_vector<ValueType> m_delb_m_1;      // delta values for bias update
  vnl_vector<ValueType> m_delb_m_2;      // delta values for bias update

  vnl_matrix<ValueType> m_InputLayerOutput;
  vnl_matrix<ValueType> m_WeightMatrix;       // composed of weights and a column of biases
  vnl_matrix<int>       m_ConnectivityMatrix;
  
  ValueType m_Momentum;
  ValueType m_Bias;
  bool      m_FirstPass;
  bool      m_SecondPass;
  ValueType m_Range;
};  //class

} // end namespace Statistics
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
  #include "itkWeightSetBase.txx"
#endif


#endif
