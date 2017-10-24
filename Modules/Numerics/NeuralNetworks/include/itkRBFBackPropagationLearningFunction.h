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
#ifndef itkRBFBackPropagationLearningFunction_h
#define itkRBFBackPropagationLearningFunction_h

#include <iostream>
#include "itkLearningFunctionBase.h"
#include "itkRBFLayer.h"

namespace itk
{
namespace Statistics
{
/** \class RBFBackPropagationLearningFunction
 * \brief This is the itkRBFBackPropagationLearningFunction class.
 *
 * \ingroup ITKNeuralNetworks
 */

template<typename LayerType, typename TTargetVector>
class ITK_TEMPLATE_EXPORT RBFBackPropagationLearningFunction : public LearningFunctionBase<LayerType, TTargetVector>
{
public:

  typedef RBFBackPropagationLearningFunction             Self;
  typedef LearningFunctionBase<LayerType, TTargetVector> Superclass;
  typedef SmartPointer<Self>                             Pointer;
  typedef SmartPointer<const Self>                       ConstPointer;

  typedef typename LayerType::LayerInterfaceType LayerInterfaceType;
  /** Method for creation through the object factory. */
  itkTypeMacro(RBFBackPropagationLearningFunction, LearningFunctionBase);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  typedef typename Superclass::ValueType ValueType;

  virtual void Learn(LayerType* layer,ValueType learningrate) ITK_OVERRIDE;
  virtual void Learn(LayerType* layer, TTargetVector error, ValueType learningrate) ITK_OVERRIDE;

  itkSetMacro(LearningRate1, ValueType);
  itkGetMacro(LearningRate1, ValueType);
  itkSetMacro(LearningRate2, ValueType);
  itkGetMacro(LearningRate2, ValueType);
  itkSetMacro(LearningRate3, ValueType);
  itkGetMacro(LearningRate3, ValueType);

protected:

  RBFBackPropagationLearningFunction();
  virtual ~RBFBackPropagationLearningFunction() ITK_OVERRIDE {};

  /** Method to print the object. */
  virtual void PrintSelf( std::ostream& os, Indent indent ) const ITK_OVERRIDE;

private:

  ValueType             m_LearningRate1;   // output weights
  ValueType             m_LearningRate2;   // centers
  ValueType             m_LearningRate3;   // widths
  vnl_vector<ValueType> m_OutputErrors;

};

} // end namespace Statistics
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkRBFBackPropagationLearningFunction.hxx"
#endif

#endif
