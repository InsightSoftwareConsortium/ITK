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
#ifndef itkCompensatedSummation_h
#define itkCompensatedSummation_h

#include "itkNumericTraits.h"
#include "itkConceptChecking.h"

namespace itk
{

/** \class CompensatedSummation
 * \brief Perform more precise accumulation of floating point numbers.
 *
 * The \c float and \c double datatypes only have finite precision.  When
 * performing a running sum of floats, the accumulated errors get progressively
 * worse as the magnitude of the sum gets large relative to new elements.
 *
 * From Wikipedia, http://en.wikipedia.org/wiki/Kahan_summation_algorithm
 *
 * "In numerical analysis, the Kahan summation algorithm (also known as
 * compensated summation) significantly reduces the numerical error in the total
 * obtained by adding a sequence of finite precision floating point numbers,
 * compared to the obvious approach. This is done by keeping a separate running
 * compensation (a variable to accumulate small errors)."
 *
 * For example, instead of
 * \code
 *   double sum = 0.0;
 *   for( unsigned int i = 0; i < array.Size(); ++i )
 *     {
 *     sum += array.GetElement(i);
 *     }
 * \endcode
 *
 * do
 *
 * \code
 *   typedef CompensatedSummation<double> CompensatedSummationType;
 *   CompensatedSummationType compensatedSummation;
 *   for( unsigned int i = 0; i < array.Size(); ++i )
 *     {
 *     compensatedSummation += array.GetElement(i);
 *     }
 *   double sum = compensatedSummation.GetSum();
 * \endcode
 *
 * \ingroup ITKCommon
 */
template < typename TFloat >
class ITK_TEMPLATE_EXPORT CompensatedSummation
{
public:
  /** Type of the input elements. */
  typedef TFloat FloatType;

  /** Type used for the sum and compensation. */
  typedef typename NumericTraits< FloatType >::AccumulateType AccumulateType;

  /** Standard class typedefs. */
  typedef CompensatedSummation Self;

  /** Constructor. */
  CompensatedSummation();

  /** Copy constructor. */
  CompensatedSummation( const Self & rhs );
  /** Assignment operator. */
  Self & operator=( const Self & rhs );

  /** Add an element to the sum. */
  void AddElement( const FloatType & element );
  Self & operator+=( const FloatType & rhs );

  /** Subtract an element from the sum. */
  Self & operator-=( const FloatType & rhs );

  /** Division and multiplication. These do not provide any numerical advantages
   * relative to vanilla division and multiplication. */
  Self & operator*=( const FloatType & rhs );
  Self & operator/=( const FloatType & rhs );

  /** Reset the sum and compensation to zero. */
  void ResetToZero();

  /** Reset the sum to the given value and the compensation to zero. */
  Self & operator=( const FloatType & rhs );

  /** Get the sum. */
  const AccumulateType & GetSum() const;

private:
  AccumulateType m_Sum;
  AccumulateType m_Compensation;

// Maybe support more types in the future with template specialization.
#ifdef ITK_USE_CONCEPT_CHECKING
  itkConceptMacro( OnlyDefinedForFloatingPointTypes, ( itk::Concept::IsFloatingPoint< TFloat > ) );
#endif // ITK_USE_CONCEPT_CHECKING
};

void ITKCommon_EXPORT CompensatedSummationAddElement( float& compensation, float& sum, const float& element );
void ITKCommon_EXPORT CompensatedSummationAddElement( double& compensation, double& sum, const double& element );

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkCompensatedSummation.hxx"
#endif

#endif
