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
#ifndef itkQuaternionRigidTransformGradientDescentOptimizer_h
#define itkQuaternionRigidTransformGradientDescentOptimizer_h

#include "itkGradientDescentOptimizer.h"
#include "ITKOptimizersExport.h"

namespace itk
{
/** \class QuaternionRigidTransformGradientDescentOptimizer
 * \brief Implement a gradient descent optimizer
 *
 * QuaternionRigidTransformGradientDescentOptimizer is an extension to the
 * simple gradient descent optimizer implemented in GradientDescentOptimizer.
 * At each iteration the current position is updated according to
 *
 * p(n+1) = p(n) + learningRate * d f(p(n)) / d p(n)
 *
 * \f[
 *        p_{n+1} = p_n
 *                + \mbox{learningRate}
 *                \, \frac{\partial f(p_n) }{\partial p_n}
 * \f]
 *
 * The learning rate is a fixed scalar defined via SetLearningRate().
 * The optimizer steps through a user defined number of iterations;
 * no convergence checking is done.
 * The first four components of p are assumed to be the four components
 * of the quaternion. After each update, the quaternion is normalized to
 * have a magnitude of one. This ensures the the transform is purely rigid.
 *
 * \sa GradientDescentOptimizer
 * \ingroup Numerics Optimizers
 * \ingroup ITKOptimizers
 */
class ITKOptimizers_EXPORT QuaternionRigidTransformGradientDescentOptimizer : public GradientDescentOptimizer
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(QuaternionRigidTransformGradientDescentOptimizer);

  /** Standard class type aliases. */
  using Self = QuaternionRigidTransformGradientDescentOptimizer;
  using Superclass = GradientDescentOptimizer;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(QuaternionRigidTransformGradientDescentOptimizer, GradientDescentOptimizer);

  /**  Parameters type.
   *  It defines a position in the optimization search space. */
  using ParametersType = Superclass::ParametersType;

  /** Advance one step following the gradient direction. */
  void
  AdvanceOneStep() override;

protected:
  QuaternionRigidTransformGradientDescentOptimizer() = default;
  ~QuaternionRigidTransformGradientDescentOptimizer() override = default;
};
} // end namespace itk

#endif
