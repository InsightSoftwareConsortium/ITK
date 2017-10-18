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
#ifndef itkBackPropagationLayer_h
#define itkBackPropagationLayer_h

#include "itkCompletelyConnectedWeightSet.h"
#include "itkLayerBase.h"
#include "itkObject.h"
#include "itkMacro.h"

namespace itk
{
namespace Statistics
{
/** \class BackPropagationLayer
 * \brief This is the itkBackPropagationLayer class.
 *
 * \ingroup ITKNeuralNetworks
 */

template<typename TMeasurementVector, typename TTargetVector>
class ITK_TEMPLATE_EXPORT BackPropagationLayer : public LayerBase<TMeasurementVector, TTargetVector>
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
  virtual void SetNumberOfNodes(unsigned int numNodes) ITK_OVERRIDE;
  virtual ValueType GetInputValue(unsigned int i) const ITK_OVERRIDE;
  virtual void SetInputValue(unsigned int i, ValueType value);

  virtual ValueType GetOutputValue(unsigned int) const ITK_OVERRIDE;
  virtual void SetOutputValue(unsigned int, ValueType);

  virtual ValueType * GetOutputVector() ITK_OVERRIDE;
  void SetOutputVector(TMeasurementVector value);

  virtual void ForwardPropagate() ITK_OVERRIDE;
  virtual void ForwardPropagate(TMeasurementVector input) ITK_OVERRIDE;

  virtual void BackwardPropagate() ITK_OVERRIDE;
  virtual void BackwardPropagate(InternalVectorType errors) ITK_OVERRIDE;

  virtual void SetOutputErrorValues(TTargetVector) ITK_OVERRIDE;
  virtual ValueType GetOutputErrorValue(unsigned int node_id) const ITK_OVERRIDE;

  virtual ValueType GetInputErrorValue(unsigned int node_id) const ITK_OVERRIDE;
  virtual ValueType * GetInputErrorVector() ITK_OVERRIDE;
  virtual void SetInputErrorValue(ValueType, unsigned int node_id) ITK_OVERRIDE;

  virtual ValueType Activation(ValueType) ITK_OVERRIDE;
  virtual ValueType DActivation(ValueType) ITK_OVERRIDE;

  /** Set/Get the bias */
  itkSetMacro( Bias, ValueType );
  itkGetConstReferenceMacro( Bias, ValueType );

protected:

  BackPropagationLayer();
  virtual ~BackPropagationLayer() ITK_OVERRIDE;

  /** Method to print the object. */
  virtual void PrintSelf( std::ostream& os, Indent indent ) const ITK_OVERRIDE;

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
#include "itkBackPropagationLayer.hxx"
#endif

#endif
