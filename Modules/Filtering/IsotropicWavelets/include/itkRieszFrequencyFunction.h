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
#include <set>
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
template <typename TFunctionValue = std::complex<double>,
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
  itkTypeMacro(RieszFrequencyFunction, FrequencyFunction);

  /** Input type for the function. */
  typedef typename Superclass::InputType InputType;

  typedef typename Superclass::OutputType                            FunctionValueType;
  typedef typename Superclass::OutputType                            OutputType;
  typedef typename Superclass::OutputType                            OutputComplexType;
  typedef itk::FixedArray<OutputType, VImageDimension>               OutputComplexArrayType;
  typedef itk::FixedArray<unsigned int, VImageDimension>             IndicesFixedArrayType;
  typedef std::vector<unsigned int>                                  IndicesArrayType;
  typedef std::vector<OutputType>                                    OutputComponentsType;
  typedef std::set<IndicesArrayType, std::greater<IndicesArrayType>> SetType;

  /**
   * Compute all possible unique indices given the subIndice: (X, 0, 0, ..., 0). Where X can be any number greater than
   * 0, but probably want to use this->m_Order.
   */
  static void
  ComputeUniqueIndices(IndicesArrayType subIndice, unsigned int init, SetType & uniqueIndices);
  /**
   * Compute all the permutations from a set of uniqueIndices.
   */
  static SetType
  ComputeAllPermutations(const SetType & uniqueIndices);

  // static std::vector<IndicesArrayType> GetAllPossibleIndices();
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
  EvaluateWithIndices(const TInput & frequency_point, const IndicesFixedArrayType & indices);

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
      itkExceptionMacro(<< "Error: inputOrder = " << inputOrder << ". It has to be greater than 0.");
    }

    if (this->m_Order != inputOrder)
    {
      this->m_Order = inputOrder;
      // Invalidate m_Indices (0,0,...,0)
      this->m_Indices.Fill(0);
      this->Modified();
    }
  }

  virtual void
  SetIndices(const IndicesFixedArrayType & indices)
  {
    if (this->m_Indices == indices)
    {
      return;
    }
    // Precondition
    unsigned int sum = 0;
    for (unsigned int dim = 0; dim < VImageDimension; ++dim)
    {
      sum += indices[dim];
    }

    if (sum != this->m_Order)
    {
      itkExceptionMacro(<< "Invalid input indices = " << indices << ". The sum of its components (" << sum
                        << " must be equal to the current Order = " << this->m_Order << ".");
    }

    this->m_Indices = indices;

    // normalizeFactor: sqrt(m_Order!/(n1!n2!...nd!))
    typename OutputComplexType::value_type normalizeFactor(1);
    for (unsigned int dim = 0; dim < VImageDimension; ++dim)
    {
      normalizeFactor *= Self::Factorial(indices[dim]);
    }
    normalizeFactor = sqrt(Self::Factorial(this->m_Order) / normalizeFactor);

    std::complex<long double> longComplex(0, -1);
    // calculate (-j)^m_Order
    itkDebugMacro(<< "\n normalizeFactor = " << normalizeFactor << "\n"
                  << "extra precision: (-j)^Order: " << longComplex);
    this->m_NormalizingIndicesComplexFactor = std::pow(longComplex, static_cast<long double>(this->m_Order));
    itkDebugMacro(<< "\n (-j)^Order: " << this->m_NormalizingIndicesComplexFactor
                  << " extra precision: " << std::pow(longComplex, static_cast<long double>(this->m_Order)));
    // (-j)^{m_Order} * sqrt(m_Order!/(n1!n2!...nd!))
    this->m_NormalizingIndicesComplexFactor *= normalizeFactor;
    this->Modified();
  }

#ifdef ITK_USE_CONCEPT_CHECKING
  itkConceptMacro(OutputTypeIsComplexCheck, (Concept::IsFloatingPoint<typename TFunctionValue::value_type>));
#endif

protected:
  RieszFrequencyFunction();
  virtual ~RieszFrequencyFunction();
  virtual void
  PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(RieszFrequencyFunction);
  unsigned int          m_Order;
  IndicesFixedArrayType m_Indices;
  OutputComplexType     m_NormalizingIndicesComplexFactor;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkRieszFrequencyFunction.hxx"
#endif

#endif
