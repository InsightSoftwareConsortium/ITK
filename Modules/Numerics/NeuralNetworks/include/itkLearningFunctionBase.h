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
#ifndef itkLearningFunctionBase_h
#define itkLearningFunctionBase_h

#include "itkLightProcessObject.h"

namespace itk
{
namespace Statistics
{
/** \class LearningFunctionBase
 * \brief The LearningFunctionBase is the base class for all the learning strategies.
 *
 * These include error back propagation, bp+momentum, conjugte gradient descent, quick prop.
 *
 * This class specifies how the errors are backpropagated for a layer. They take a LayerBase
 * object as input and compute the input for the layers input weightset.
 *
 * \ingroup ITKNeuralNetworks
 */

template<typename LayerType, typename TTargetVector>
class LearningFunctionBase : public LightProcessObject
{
public:

  typedef LearningFunctionBase     Self;
  typedef LightProcessObject       Superclass;
  typedef SmartPointer<Self>       Pointer;
  typedef SmartPointer<const Self> ConstPointer;

  /** Method for creation through the object factory. */
  itkTypeMacro(LearningFunctionBase, LightProcessObject);

  typedef typename LayerType::ValueType ValueType;

  virtual void Learn(LayerType* layer, ValueType) = 0;
  virtual void Learn(LayerType* layer, TTargetVector error,ValueType) = 0;

protected:
  LearningFunctionBase() {};
  ~LearningFunctionBase() ITK_OVERRIDE {};

  /** Method to print the object. */
  virtual void PrintSelf( std::ostream& os, Indent indent ) const ITK_OVERRIDE
    {
    Superclass::PrintSelf( os, indent );
    os << indent << "LearningFunctionBase(" << this << ")" << std::endl;
    }
};

} // end namespace Statistics
} // end namespace itk

#endif
