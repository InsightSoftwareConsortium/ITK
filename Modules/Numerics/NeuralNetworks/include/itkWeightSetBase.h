/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkWeightSetBase_h
#define itkWeightSetBase_h

#include "itkLightProcessObject.h"
#include "vnl/vnl_matrix.h"
#include "vnl/vnl_diag_matrix.h"
#include "itkVector.h"
#include "itkMersenneTwisterRandomVariateGenerator.h"
#include <cmath>
#include <cstdlib>

namespace itk
{
namespace Statistics
{
/** \class WeightSetBase
 * \brief This is the itkWeightSetBase class.
 *
 * \ingroup ITKNeuralNetworks
 */

template<typename TMeasurementVector, typename TTargetVector>
class ITK_TEMPLATE_EXPORT WeightSetBase : public LightProcessObject
{
public:

  typedef WeightSetBase            Self;
  typedef LightProcessObject       Superclass;
  typedef SmartPointer<Self>       Pointer;
  typedef SmartPointer<const Self> ConstPointer;
  itkTypeMacro(WeightSetBase, LightProcessObject);

  typedef MersenneTwisterRandomVariateGenerator  RandomVariateGeneratorType;

  typedef typename TMeasurementVector::ValueType ValueType;
  typedef ValueType*                             ValuePointer;
  typedef const ValueType*                       ValueConstPointer;

  void Initialize();

  ValueType RandomWeightValue(ValueType low, ValueType high);

  virtual void ForwardPropagate(ValuePointer inputlayeroutputvalues);
  virtual void BackwardPropagate(ValuePointer inputerror);

  void SetConnectivityMatrix(vnl_matrix < int>);

  void SetNumberOfInputNodes(unsigned int n);
  unsigned int GetNumberOfInputNodes() const;

  void SetNumberOfOutputNodes(unsigned int n);
  unsigned int GetNumberOfOutputNodes() const;

  void SetRange(ValueType Range);

  virtual ValuePointer GetOutputValues();
  virtual ValuePointer GetInputValues();

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
  ValueConstPointer GetWeightValues() const;

  void SetWeightValues(ValuePointer weights);
  virtual void UpdateWeights(ValueType LearningRate);

  itkSetMacro( Momentum, ValueType );
  itkGetConstReferenceMacro( Momentum, ValueType );

  itkSetMacro( Bias, ValueType );
  itkGetConstReferenceMacro( Bias, ValueType );

  itkSetMacro( FirstPass, bool );
  itkGetConstMacro( FirstPass, bool );

  itkSetMacro( SecondPass, bool );
  itkGetConstMacro( SecondPass, bool );

  void InitializeWeights();

  itkSetMacro(WeightSetId,unsigned int);
  itkGetConstMacro(WeightSetId,unsigned int);

  itkSetMacro(InputLayerId,unsigned int);
  itkGetConstMacro(InputLayerId,unsigned int);

  itkSetMacro(OutputLayerId,unsigned int);
  itkGetConstMacro(OutputLayerId,unsigned int);

protected:

  WeightSetBase();
  ~WeightSetBase() ITK_OVERRIDE;

  /** Method to print the object. */
  virtual void PrintSelf( std::ostream& os, Indent indent ) const ITK_OVERRIDE;

  typename RandomVariateGeneratorType::Pointer m_RandomGenerator;

  unsigned int              m_NumberOfInputNodes;
  unsigned int              m_NumberOfOutputNodes;
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

  vnl_matrix<ValueType> m_Del;           // dw=lr * del * y
  vnl_matrix<ValueType> m_Del_new;       // dw=lr * del * y
  vnl_matrix<ValueType> m_Del_m_1;       // dw=lr * del * y
  vnl_matrix<ValueType> m_Del_m_2;       // dw=lr * del * y

  vnl_vector<ValueType> m_Delb;          // delta values for bias update
  vnl_vector<ValueType> m_Delb_new;      // delta values for bias update
  vnl_vector<ValueType> m_Delb_m_1;      // delta values for bias update
  vnl_vector<ValueType> m_Delb_m_2;      // delta values for bias update

  vnl_matrix<ValueType> m_InputLayerOutput;
  vnl_matrix<ValueType> m_WeightMatrix;  // composed of weights and a column
  // of biases
  vnl_matrix<int>       m_ConnectivityMatrix;

  ValueType m_Momentum;
  ValueType m_Bias;
  bool      m_FirstPass;
  bool      m_SecondPass;
  ValueType m_Range;

  unsigned int m_InputLayerId;
  unsigned int m_OutputLayerId;
  unsigned int m_WeightSetId;

};  //class

} // end namespace Statistics
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkWeightSetBase.hxx"
#endif


#endif
