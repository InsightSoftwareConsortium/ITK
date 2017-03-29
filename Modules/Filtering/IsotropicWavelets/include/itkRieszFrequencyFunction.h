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
#include <complex>
#include <numeric>

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

  typedef typename Superclass::OutputType FunctionValueType;
  /** Output type for the function. */
  typedef std::complex<typename Superclass::OutputType>                                   OutputComplexType;
  typedef itk::FixedArray<std::complex<typename Superclass::OutputType>, VImageDimension> OutputComplexArrayType;
  typedef itk::FixedArray<unsigned int, VImageDimension>                                  IndicesArrayType;

  /** Evaluate the function at a given frequency point. */
  virtual FunctionValueType
  Evaluate(const TInput &) const ITK_OVERRIDE
  {
    itkExceptionMacro("Evaluate(TInput&) is not valid for RieszFrequencyFunction."
                      "Use EvaluateArray instead, returning an OutputComplexArrayType,"
                      "or Evaluate(point, direction) that returns a complex value .");
  };
  /** Calculate first order (m_Order = 1) riesz function at indicated direction.
   * Equivalent to:
   * EvaluateWithIndices(frequency_point, indices) with:
   * m_Order = 1,
   * indices = 1 at index == direction, 0 elsewhere.
   */
  virtual OutputComplexType
  Evaluate(const TInput & frequency_point, const unsigned int & direction) const;

  virtual OutputComplexArrayType
  EvaluateArray(const TInput & frequency_point) const;

  virtual OutputComplexType
  EvaluateWithIndices(const TInput & frequency_point, const IndicesArrayType & indices) const;

  /** Calculate magnitude (euclidean norm) of input point. **/
  inline double
  Magnitude(const TInput & point) const
  {
    return sqrt(std::inner_product(point.Begin(), point.End(), point.Begin(), 0.0));
  }

  /// Factorial
  static long
  Factorial(long n)
  {
    if (n <= 1)
    {
      return 1;
    }
    else
    {
      return n * Self::Factorial(n - 1);
    }
  }

  /** Order of the generalized riesz transform. */
  itkGetConstReferenceMacro(Order, unsigned int);
  virtual void
  SetOrder(const unsigned int inputOrder)
  {
    // Precondition
    if (inputOrder < 1)
    {
      itkExceptionMacro(<< "Error: inputOrder = " << inputOrder << ". It has to be greater than 0.")
    }

    if (this->m_Order != inputOrder)
    {
      this->m_Order = inputOrder;
      this->m_OrderFactorial = static_cast<unsigned long>(Self::Factorial(inputOrder));
      this->Modified();
    }
  }

protected:
  RieszFrequencyFunction();
  virtual ~RieszFrequencyFunction();
  virtual void
  PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(RieszFrequencyFunction);
  unsigned int  m_Order;
  unsigned long m_OrderFactorial;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkRieszFrequencyFunction.hxx"
#endif

#endif
