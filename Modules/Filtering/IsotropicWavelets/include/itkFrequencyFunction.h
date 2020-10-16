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
#ifndef itkFrequencyFunction_h
#define itkFrequencyFunction_h

#include <itkSpatialFunction.h>
#include <itkFloatTypes.h>

namespace itk
{
/** \class FrequencyFunction
 * Abstract / Interface class for FrequencyFunction.
 *
 * \ingroup SpatialFunctions
 * \ingroup IsotropicWavelets
 */
template <typename TFunctionValue = double,
          unsigned int VImageDimension = 3,
          typename TInput = Point<SpacePrecisionType, VImageDimension>>
class FrequencyFunction : public SpatialFunction<TFunctionValue, VImageDimension, TInput>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(FrequencyFunction);

  /** Standard class type alias. */
  using Self = FrequencyFunction;
  using Superclass = SpatialFunction<TFunctionValue, VImageDimension, TInput>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Run-time type information (and related methods). */
  itkTypeMacro(FrequencyFunction, SpatialFunction);

  /** Input type for the function. */
  using InputType = typename Superclass::InputType;

  /** Output type for the function. */
  using FunctionValueType = typename Superclass::OutputType;
  using OutputType = typename Superclass::OutputType;

  /**
   * The evaluate function require frequency in Hertz (1/s)
   * \f$ w[Hz] = \frac{ w[rad/s]}{2\pi}\f$
   */
  inline FunctionValueType
  RadPerSecToHertz(const TFunctionValue & w_rad_per_sec) const
  {
    return w_rad_per_sec / (2 * itk::Math::pi);
  };
  /** Evaluate the function at a given frequency point. */
  FunctionValueType
  Evaluate(const TInput & frequency_point) const override = 0;

protected:
  FrequencyFunction() = default;
  ~FrequencyFunction() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override
  {
    Superclass::PrintSelf(os, indent);
  }
};
} // end namespace itk

#endif
