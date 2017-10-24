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
#ifndef itkQuickPropLearningRule_h
#define itkQuickPropLearningRule_h

#include <iostream>
#include "itkLearningFunctionBase.h"

namespace itk
{
namespace Statistics
{
/** \class QuickPropLearningRule
 * \brief The QuickPropLearningRule is the base class for all
 * the ErrorBackPropagationLearning strategies.
 *
 * ErrorBackPropagationLearning strategies include error back propagation,
 * bp+momentum, conjugte gradient descent, and quick prop.
 *
 * This class specifies how the errors are backpropagated for a layer.
 * They take a LayerBase object as input and compute the input for the
 * layers input weightset.
 *
 * \ingroup ITKNeuralNetworks
 */

template<typename LayerType, typename TTargetVector>
class ITK_TEMPLATE_EXPORT QuickPropLearningRule : public LearningFunctionBase<LayerType, TTargetVector>
{
public:

  typedef QuickPropLearningRule                          Self;
  typedef LearningFunctionBase<LayerType, TTargetVector> Superclass;
  typedef SmartPointer<Self>                             Pointer;
  typedef SmartPointer<const Self>                       ConstPointer;

  /** Method for creation through the object factory. */
  itkTypeMacro(QuickPropLearningRule, LearningFunctionBase);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  typedef typename Superclass::ValueType ValueType;
  virtual void Learn(LayerType* layer, ValueType learningrate) ITK_OVERRIDE;
  virtual void Learn(LayerType* layer, TTargetVector errors, ValueType learningrate) ITK_OVERRIDE;

  itkSetMacro(Max_Growth_Factor, ValueType);
  itkGetConstReferenceMacro(Max_Growth_Factor,ValueType);

  itkSetMacro(Decay, ValueType);
  itkGetConstReferenceMacro(Decay,ValueType);

protected:
  QuickPropLearningRule();
  virtual ~QuickPropLearningRule() ITK_OVERRIDE{};

  ValueType m_Momentum;
  ValueType m_Max_Growth_Factor;
  ValueType m_Decay;
  ValueType m_Threshold;
  ValueType m_Epsilon;

  /** Method to print the object. */
  virtual void PrintSelf( std::ostream& os, Indent indent ) const ITK_OVERRIDE;
};

} // end namespace Statistics
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
  #include "itkQuickPropLearningRule.hxx"
#endif

#endif
