/*=========================================================================
 *
 *  Copyright NumFOCUS
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
#ifndef itkVersorRigid3DTransformOptimizer_h
#define itkVersorRigid3DTransformOptimizer_h

#include "itkRegularStepGradientDescentOptimizer.h"
#include "itkVersor.h"
#include "ITKOptimizersExport.h"

namespace itk
{
/** \class VersorRigid3DTransformOptimizer
 * \brief Implement a gradient descent optimizer for the VersorRigid3DTransform
 * parameter space.
 *
 * VersorRigid3DTransformOptimizer is a variant of the gradient descent
 * optimizer implemented in RegularStepGradientDescentOptimizer.
 *
 * Versors are not in a vector space, for that reason, the classical gradient
 * descent algorithm has to be modified in order to be applicable to Versors
 * (unit quaternions) that form the group SO(3).
 *
 * The Versor space has only three degrees of freedom, even though Versors are
 * represented using four values.
 *
 * This optimizer assumes that the CostFunction to be optimized has an
 * itk::Versor and an itk::Vector as parameters.
 *
 * \sa RegularStepGradientDescentOptimizer
 * \sa Versor
 * \sa VersorRigid3DTransform
 *
 * \ingroup Numerics Optimizers
 * \ingroup ITKOptimizers
 */
class ITKOptimizers_EXPORT VersorRigid3DTransformOptimizer : public RegularStepGradientDescentBaseOptimizer
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(VersorRigid3DTransformOptimizer);

  /** Standard class type aliases. */
  using Self = VersorRigid3DTransformOptimizer;
  using Superclass = RegularStepGradientDescentBaseOptimizer;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(VersorRigid3DTransformOptimizer, RegularStepGradientDescentBaseOptimizer);

  /** This class is specialized for 3D  */
  static constexpr unsigned int SpaceDimension = 6;

  /**  Versor Type  */
  using VersorType = Versor<double>;
  using VectorType = VersorType::VectorType;

  /** Advance one step following the gradient direction. */
  void
  StepAlongGradient(double factor, const DerivativeType & transformedGradient) override;

protected:
  VersorRigid3DTransformOptimizer() = default;
  ~VersorRigid3DTransformOptimizer() override = default;
};
} // end namespace itk

#endif
