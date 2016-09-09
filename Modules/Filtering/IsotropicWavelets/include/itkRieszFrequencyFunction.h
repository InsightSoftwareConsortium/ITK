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
#ifndef itkRieszFrequencyFunction_h
#define itkRieszFrequencyFunction_h

#include "itkFrequencyFunction.h"

namespace itk
{
/** \class RieszFrequencyFunction
 * Riesz function is a Hilbert transform for N-Dimension signal.
 *
 * \ingroup SpatialFunctions
 * \ingroup IsotropicWavelets
 */
template <typename TFunctionValue = double,
          unsigned int VImageDimension = 3,
          typename TInput = Point<SpacePrecisionType, VImageDimension>>
class RieszFrequencyFunction : public FrequencyFunction<TFunctionValue, VImageDimension, TInput>
{
public:
  /** Standard class typedefs. */
  typedef RieszFrequencyFunction                                   Self;
  typedef SpatialFunction<TFunctionValue, VImageDimension, TInput> Superclass;
  typedef SmartPointer<Self>                                       Pointer;
  typedef SmartPointer<const Self>                                 ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(RieszFrequencyFunction, SpatialFunction);

  /** Input type for the function. */
  typedef typename Superclass::InputType InputType;

  /** Output type for the function. */
  typedef typename Superclass::OutputType FunctionValueType;
  typedef typename Superclass::OutputType OutputType;

  /** Evaluate the function at a given frequency point. */
  virtual FunctionValueType
  Evaluate(const TInput &) const ITK_OVERRIDE
  {
    itkExceptionMacro("Evaluate(TInput&) is not valid for RieszFrequencyFunction."
                      "Use EvaluateArray instead, returning an array type,"
                      "or Evaluate(point, dimension) that returns a scalar.");
  };
  FunctionValueType
  Evaluate(const TInput & frequency_point, const unsigned int & dimension) const;
  InputType
  EvaluateArray(const TInput & frequency_point) const;

  /** Calculate magnitude (euclidean norm) of input point. **/
  inline double
  Magnitude(const TInput & point) const
  {
    double accum(0);
    for (size_t d = 0; d < VImageDimension; ++d)
    {
      accum += point[d] * point[d];
    }
    return sqrt(accum);
  }

protected:
  RieszFrequencyFunction();
  virtual ~RieszFrequencyFunction();
  virtual void
  PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(RieszFrequencyFunction);
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkRieszFrequencyFunction.hxx"
#endif

#endif
