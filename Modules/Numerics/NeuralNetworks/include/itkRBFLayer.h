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
#ifndef itkRBFLayer_h
#define itkRBFLayer_h

#include "itkCompletelyConnectedWeightSet.h"
#include "itkLayerBase.h"
#include "itkObject.h"
#include "itkMacro.h"
#include "itkRadialBasisFunctionBase.h"
#include "itkEuclideanDistanceMetric.h"

namespace itk
{
namespace Statistics
{
/** \class RBFLayer
 * \brief This is the itkRBFLayer class.
 *
 * \ingroup ITKNeuralNetworks
 */

template<typename TMeasurementVector, typename TTargetVector>
class RBFLayer : public LayerBase<TMeasurementVector, TTargetVector>
{
public:
  typedef RBFLayer                                     Self;
  typedef LayerBase<TMeasurementVector, TTargetVector> Superclass;
  typedef SmartPointer<Self>                           Pointer;
  typedef SmartPointer<const Self>                     ConstPointer;

  /** Method for creation through the object factory. */
  itkTypeMacro(RBFLayer, LayerBase);
  itkNewMacro(Self);

  typedef typename Superclass::ValueType          ValueType;
  typedef typename Superclass::ValuePointer       ValuePointer;
  typedef vnl_vector<ValueType>                   NodeVectorType;
  typedef typename Superclass::InternalVectorType InternalVectorType;
  typedef typename Superclass::OutputVectorType   OutputVectorType;
  typedef typename Superclass::LayerInterfaceType LayerInterfaceType;
  typedef CompletelyConnectedWeightSet<TMeasurementVector,TTargetVector>
                                                  WeightSetType;

  typedef typename Superclass::WeightSetInterfaceType        WeightSetInterfaceType;
  typedef typename Superclass::InputFunctionInterfaceType    InputFunctionInterfaceType;
  typedef typename Superclass::TransferFunctionInterfaceType TransferFunctionInterfaceType;

  //Distance Metric
  typedef EuclideanDistanceMetric<InternalVectorType> DistanceMetricType;
  typedef typename DistanceMetricType::Pointer        DistanceMetricPointer;
  typedef RadialBasisFunctionBase<ValueType>          RBFType;

  //Member Functions
  itkGetConstReferenceMacro(RBF_Dim, unsigned int);
  void SetRBF_Dim(unsigned int size);
  virtual void SetNumberOfNodes(unsigned int numNodes);
  virtual ValueType GetInputValue(unsigned int i) const;
  void SetInputValue(unsigned int i, ValueType value);

  virtual ValueType GetOutputValue(unsigned int) const;
  virtual void SetOutputValue(unsigned int, ValueType);

  virtual ValueType * GetOutputVector();
  void SetOutputVector(TMeasurementVector value);

  virtual void ForwardPropagate();
  virtual void ForwardPropagate(TMeasurementVector input);

  virtual void BackwardPropagate();
  virtual void BackwardPropagate(TTargetVector itkNotUsed(errors)){};

  virtual void SetOutputErrorValues(TTargetVector);
  virtual ValueType GetOutputErrorValue(unsigned int node_id) const;

  virtual ValueType GetInputErrorValue(unsigned int node_id) const;
  virtual ValueType * GetInputErrorVector();
  virtual void SetInputErrorValue(ValueType, unsigned int node_id);

  //TMeasurementVector GetCenter(int i);
  InternalVectorType GetCenter(unsigned int i) const;
  void SetCenter(TMeasurementVector c,unsigned int i);

  ValueType GetRadii(unsigned int i) const;
  void SetRadii(ValueType c,unsigned int i);

  virtual ValueType Activation(ValueType);
  virtual ValueType DActivation(ValueType);

  /** Set/Get the bias */
  itkSetMacro( Bias, ValueType );
  itkGetConstReferenceMacro( Bias, ValueType );

  void SetDistanceMetric(DistanceMetricType* f);
  itkGetModifiableObjectMacro(DistanceMetric, DistanceMetricType );

  itkSetMacro(NumClasses,unsigned int);
  itkGetConstReferenceMacro(NumClasses,unsigned int);

  void SetRBF(RBFType* f);
  itkGetModifiableObjectMacro(RBF, RBFType);

protected:

  RBFLayer();
  virtual ~RBFLayer();

  /** Method to print the object. */
  virtual void PrintSelf( std::ostream& os, Indent indent ) const;

private:

  NodeVectorType   m_NodeInputValues;
  NodeVectorType   m_NodeOutputValues;
  NodeVectorType   m_InputErrorValues;
  NodeVectorType   m_OutputErrorValues;

  typename DistanceMetricType::Pointer  m_DistanceMetric;

  std::vector<InternalVectorType>       m_Centers;  // ui....uc
  InternalVectorType                    m_Radii;
  unsigned int                          m_NumClasses;
  ValueType                             m_Bias;
  unsigned int                          m_RBF_Dim;
  typename RBFType::Pointer             m_RBF;
};

} // end namespace Statistics
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkRBFLayer.hxx"
#endif

#endif
