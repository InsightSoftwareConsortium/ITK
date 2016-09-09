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
  /** Standard class typedefs. */
  typedef FrequencyFunction                                        Self;
  typedef SpatialFunction<TFunctionValue, VImageDimension, TInput> Superclass;
  typedef SmartPointer<Self>                                       Pointer;
  typedef SmartPointer<const Self>                                 ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(FrequencyFunction, SpatialFunction);

  /** Input type for the function. */
  typedef typename Superclass::InputType InputType;

  /** Output type for the function. */
  typedef typename Superclass::OutputType FunctionValueType;
  typedef typename Superclass::OutputType OutputType;

  /**
   * The evaluate function require frequency in Hertz (1/s)
   * $w[Hz] = \frac{ w[rad/s]}{2\pi}$
   */
  inline FunctionValueType
  RadPerSecToHertz(const TFunctionValue & w_rad_per_sec) const
  {
    return w_rad_per_sec / (2 * itk::Math::pi);
  };
  /** Evaluate the function at a given frequency point. */
  virtual FunctionValueType
  Evaluate(const TInput & frequency_point) const ITK_OVERRIDE = 0;

protected:
  FrequencyFunction() {};
  virtual ~FrequencyFunction() {};
  virtual void
  PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE
  {
    Superclass::PrintSelf(os, indent);
  }

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(FrequencyFunction);
};
} // end namespace itk

#endif
