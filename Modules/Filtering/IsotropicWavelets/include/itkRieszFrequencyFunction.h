/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
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
#include <set>
#include <complex>
#include <numeric>
#include <functional>
#include "itkRieszUtilities.h"

namespace itk
{
/** \class RieszFrequencyFunction
 * Riesz function is a Hilbert transform for N-Dimension signal.
 *
 * \ingroup SpatialFunctions
 * \ingroup IsotropicWavelets
 */
template <typename TFunctionValue = std::complex<double>,
          unsigned int VImageDimension = 3,
          typename TInput = Point<SpacePrecisionType, VImageDimension>>
class RieszFrequencyFunction : public FrequencyFunction<TFunctionValue, VImageDimension, TInput>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(RieszFrequencyFunction);

  /** Standard class type alias. */
  using Self = RieszFrequencyFunction;
  using Superclass = SpatialFunction<TFunctionValue, VImageDimension, TInput>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(RieszFrequencyFunction, FrequencyFunction);

  /** Input type for the function. */
  using InputType = typename Superclass::InputType;

  using FunctionValueType = typename Superclass::OutputType;
  using OutputType = typename Superclass::OutputType;
  using OutputComplexType = typename Superclass::OutputType;

  /** Indices Type, user needs to resize it to VImageDimension.
   *  ITK_BUG, dev: it would be better to use std::array or fix itk::FixedArray::ReverseIterator has no operator-, used
   * by std::sort ...
   */
  using IndicesArrayType = std::vector<unsigned int>;
  using OutputComponentsType = std::vector<OutputComplexType>;
  using SetType = std::set<IndicesArrayType, std::greater<IndicesArrayType>>;
  using OutputComplexArrayType = itk::FixedArray<OutputComplexType, VImageDimension>;

  /**
   * Compute number of components p(N, d), where N = Order, d = Dimension.
   * p(N,d) = (N + d - 1)!/( (d-1)! N! )
   *
   * @param order N or this->m_Order
   *
   * @return NumberOfComponents given the order.
   */
  static unsigned int
  ComputeNumberOfComponents(const unsigned int & order)
  {
    return itk::utils::ComputeNumberOfComponents(order, VImageDimension);
  }

  /**
   * Compute all possible unique indices given the subIndex: (X, 0, ..., 0).
   * Where X can be any number greater than 0, but probably want to use this->m_Order.
   *
   * @param subIndex Indice (X,0,...,0) where X > 0.
   * @param uniqueIndices Reference to set that store results.
   * @param init position to evaluate  subIndex. Needed for recursion purposes.
   */
  static void
  ComputeUniqueIndices(IndicesArrayType subIndex, SetType & uniqueIndices, unsigned int init = 0)
  {
    itk::utils::ComputeUniqueIndices<IndicesArrayType, VImageDimension>(subIndex, uniqueIndices, init);
  }

  /**
   * Compute all the permutations from a set of uniqueIndices.
   */
  static SetType
  ComputeAllPermutations(const SetType & uniqueIndices)
  {
    return itk::utils::ComputeAllPermutations<IndicesArrayType>(uniqueIndices);
  }

  /**
   * Compute all the possible indices for a given order.
   */
  static SetType
  ComputeAllPossibleIndices(const unsigned int & order)
  {
    return itk::utils::ComputeAllPossibleIndices<IndicesArrayType, VImageDimension>(order);
  }

  /** Evaluate the function at a given frequency point. */
  FunctionValueType
  Evaluate(const TInput &) const override
  {
    itkExceptionMacro("Evaluate(TInput&) is not valid for RieszFrequencyFunction."
                      "Use EvaluateWithIndices(point, indices) or EvaluateAllComponents(point)");
  };

  /**
   * Compute RieszTransform component based on indices. It takes into account m_Order.
   *
   * @param frequency_point w (array)
   * @param indices
   * Precondition: sum(n1,n2,...,nd) = N (m_Order)
   * n = (n1,n2,...nd) := indices vector. d-array, where d is the Dimension of the image.
   *
   * @return R^n = (-j)^N * sqrt(N!/(n1!*n2!...nd!)) * (w1^n1 * w2^n2 * ... wd^nd) * 1.0 / ||w||^N
   * R^n = complex_component * normalizing_factor * frequency_factor * 1.0/freq_magnitude_factor
   */
  virtual OutputComplexType
  EvaluateWithIndices(const TInput & frequency_point, const IndicesArrayType & indices);

  /**
   * Evaluate the frequency point and return the value for all the components of the generalized Riesz transform.
   * The number of components depends on the value of m_Order.
   * \sa ComputeAllPermutations
   *
   * @param frequency_point point in the frequency space.
   *
   * @return vector holding the values for all the components at the frequency point.
   */
  virtual OutputComponentsType
  EvaluateAllComponents(const TInput & frequency_point) const;

  /**
   * Compute normalizing factor given an index = (n1,n2,...,nVImageDimension)
   * Also takes into account this->m_Order = N
   * @param indices input indices.
   *
   * @return normalizing factor = (-j)^N * sqrt(N!/(n1!*n2!...nd!))
   */
  OutputComplexType
  ComputeNormalizingFactor(const IndicesArrayType & indices) const;

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
    return itk::utils::Factorial(n);
  }

  /** Order of the generalized riesz transform. */
  virtual void
  SetOrder(const unsigned int inputOrder)
  {
    // Precondition
    if (inputOrder < 1)
    {
      itkExceptionMacro(<< "Error: inputOrder = " << inputOrder << ". It has to be greater than 0.");
    }

    if (this->m_Order != inputOrder)
    {
      this->m_Order = inputOrder;
      // Calculate all the possible indices.
      this->m_Indices = Self::ComputeAllPossibleIndices(this->m_Order);
      this->Modified();
    }
  }
  itkGetConstReferenceMacro(Order, unsigned int);

  /** Sorted set of indices. All permutations allowed by Order.
   * Calculated when SetOrder */
  itkGetConstReferenceMacro(Indices, SetType);

#ifdef ITK_USE_CONCEPT_CHECKING
  itkConceptMacro(OutputTypeIsComplexCheck, (Concept::IsFloatingPoint<typename TFunctionValue::value_type>));
#endif

protected:
  RieszFrequencyFunction();
  ~RieszFrequencyFunction() override;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

private:
  unsigned int m_Order{ 0 };
  SetType      m_Indices;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkRieszFrequencyFunction.hxx"
#endif

#endif
