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
#ifndef itkSinusoidSpatialFunction_h
#define itkSinusoidSpatialFunction_h

#include "itkSpatialFunction.h"
#include "itkFixedArray.h"
#include "itkFloatTypes.h"


namespace itk
{
/** \class SinusoidSpatialFunction
 * \brief N-dimensional sinusoid spatial function class
 *
 * \f[
 *   I(\mathbf{x}) = \cos(2\pi\sum_i \omega_i x_i + \phi)
 * \f]
 *
 * where \f$\omega_i\f$ is the frequency, in spatial units, in direction
 * \f$i\f$, and \f$\phi\f$ is a phase shift.
 *
 * \sa SinusoidImageSource
 *
 * \ingroup SpatialFunctions
 * \ingroup PhaseSymmetry
 */

template <typename TOutput = double,
          unsigned int VImageDimension = 3,
          typename TInput = Point<SpacePrecisionType, VImageDimension>>
class SinusoidSpatialFunction : public SpatialFunction<TOutput, VImageDimension, TInput>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(SinusoidSpatialFunction);

  /** Standard class type alias. */
  using Self = SinusoidSpatialFunction;
  using Superclass = SpatialFunction<TOutput, VImageDimension, TInput>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(SinusoidSpatialFunction, SpatialFunction);

  /** Input type for the function. */
  using InputType = typename Superclass::InputType;

  /** Output type for the function. */
  using OutputType = typename Superclass::OutputType;

  /** Type used to store gaussian parameters. */
  using ArrayType = FixedArray<double, VImageDimension>;

  /** Evaluate the function at a given position. */
  OutputType
  Evaluate(const TInput & position) const override;

  /** Set/Get the sinusoid phase shift in radians. */
  itkSetMacro(PhaseOffset, double);
  itkGetConstMacro(PhaseOffset, double);
  /** Set/Get the per-direction frequency in cycles / spatial unit. */
  itkSetMacro(Frequency, ArrayType);
  itkGetConstReferenceMacro(Frequency, ArrayType);

protected:
  SinusoidSpatialFunction();
  ~SinusoidSpatialFunction() override;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

private:
  /** The spatial frequency in each direction. */
  ArrayType m_Frequency;

  /** The phase shift. */
  double m_PhaseOffset{ 0.0 };
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkSinusoidSpatialFunction.hxx"
#endif

#endif
