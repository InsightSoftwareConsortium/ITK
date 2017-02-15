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
#ifndef itkIsotropicFrequencyFunction_h
#define itkIsotropicFrequencyFunction_h

#include <itkFrequencyFunction.h>
#include <itkFloatTypes.h>

namespace itk
{
/** \class IsotropicFrequencyFunction
 * Abstract-Interface class for \sa FrequencyFunction that are isotropic,
 * so Evaluate(frequency_point) only depends on the magnitude of the vector/point.
 * Evaluate(frequency_point) just calls EvaluateMagnitude(frequency_norm) that has
 * to be implemented in a derived class.
 *
 * \sa FrequencyFunction
 * \ingroup SpatialFunctions
 * \ingroup IsotropicWavelets
 */
template <typename TFunctionValue = double,
          unsigned int VImageDimension = 3,
          typename TInput = Point<SpacePrecisionType, VImageDimension>>
class IsotropicFrequencyFunction : public FrequencyFunction<TFunctionValue, VImageDimension, TInput>
{
public:
  /** Standard class typedefs. */
  typedef IsotropicFrequencyFunction                                 Self;
  typedef FrequencyFunction<TFunctionValue, VImageDimension, TInput> Superclass;
  typedef SmartPointer<Self>                                         Pointer;
  typedef SmartPointer<const Self>                                   ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(IsotropicFrequencyFunction, FrequencyFunction);

  /** Input type for the function. */
  typedef typename Superclass::InputType InputType;

  /** Output type for the function. */
  typedef typename Superclass::FunctionValueType FunctionValueType;
  typedef typename Superclass::OutputType        OutputType;
  /** Calculate magnitude (euclidean norm ) of input point. **/
  inline double
  Magnitude(const TInput & point) const
  {
    double accum(0);

    for (size_t d = 0; d < VImageDimension; ++d)
    {
      accum += point[d] * point[d];
    }
    return sqrt(accum);
  };

  /** Evaluate the function at a given frequency point. Because the function is istropic, this calls the
   * EvaluateMagnitude with the magnitude of the point. */
  virtual FunctionValueType
  Evaluate(const TInput & frequency_point) const ITK_OVERRIDE
  {
    return this->EvaluateMagnitude(static_cast<TFunctionValue>(this->Magnitude(frequency_point)));
  }

  /** Evaluate the function given the magnitude (euclidean norm) of the frequency point. This method is call by
   * Evaluate, and has to be implemented on derived class. */
  virtual FunctionValueType
  EvaluateMagnitude(const TFunctionValue & freq_norm_in_hz) const = 0;

  // #<{(|*** Forward/Analysis **|)}>#
  // virtual FunctionValueType EvaluateForward(const TFunctionValue& freq_in_hz) const = 0;
  // #<{(|*** Inverse/Synthesis **|)}>#
  // virtual FunctionValueType EvaluateInverse(const TFunctionValue& freq_in_hz) const = 0;

protected:
  IsotropicFrequencyFunction() {};
  virtual ~IsotropicFrequencyFunction() {};
  virtual void
  PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE
  {
    Superclass::PrintSelf(os, indent);
  }

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(IsotropicFrequencyFunction);
};
} // end namespace itk

#endif
