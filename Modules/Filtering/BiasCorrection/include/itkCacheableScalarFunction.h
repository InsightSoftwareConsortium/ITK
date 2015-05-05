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
#ifndef itkCacheableScalarFunction_h
#define itkCacheableScalarFunction_h

#include "itkArray.h"
#include "itkIntTypes.h"
#include "ITKBiasCorrectionExport.h"

namespace itk
{
/** \class CacheableScalarFunction
 * \brief function cache implementation
 *
 * This is the base class for continuous scalar functions which
 * needs cache for their pre-evaluated function returns.
 *
 * The internal cache is created using the upper- and lower-bound domain
 * values of the functional form (f(x))of subclasses of this class. So the
 * cache only stores pre-evaluated values between f(lower-bound) and
 * f(upper-bound).
 *
 * To create a cache for continuous function, it uses sampling.
 * With the given sample number , upper-bound, and lower-bound, it calculates
 * interval within the ranges. It pre-evaluates and save f(x)
 * where x = lower-bound + interval * [0 - sample number]
 *
 * If a subclass of this class want to use a cache, it should
 * explicitly call CreateCache(...) member function. GetCachedValue(x) will
 * return pre-evaluated f(x) value. However, the return value from
 * GetCachedValue(x) might be different from the exact return value from f(x)
 * which is Evaluate(x) member function of subclasses of this class, because
 * The GetCachedValue(x) member function internally converts x to cache table
 * index and the conversion involves with truncation. So, users can think the
 * cached value as an approximate to exact function return.
 *
 * In some case, approximate values can be useful.
 * For example, CompositeValleyFunction can be used as an M-estimator and
 * it is currently used for MRIBiasFieldCorrectionFilter
 * as an energy function. The bias field estimation requires calculation of
 * energy values again and again for each iteration.
 * \ingroup ITKBiasCorrection
 */
class ITKBiasCorrection_EXPORT CacheableScalarFunction
{
public:
  /** Constructor. */
  CacheableScalarFunction();

  /** Destructor. */
  virtual ~CacheableScalarFunction();

  /** Function's input and output value type. */
  typedef double               MeasureType;
  typedef Array< MeasureType > MeasureArrayType;

  /** Get the number of samples between the lower-bound and upper-bound
   * of the cache table. */
  SizeValueType GetNumberOfSamples() { return m_NumberOfSamples; }

  /** Check if the internal cache table and its values are valid. */
  bool IsCacheAvailable() { return m_CacheAvailable; }

  /** Get the upper-bound of domain that is used for filling the cache table. */
  double GetCacheUpperBound() { return m_CacheUpperBound; }

  /** Get the lower-bound of domain that is used for filling the cache table. */
  double GetCacheLowerBound() { return m_CacheLowerBound; }

  /** y = f(x)
   * Subclasses of this class should override this member function
   * to provide their own functional operation . */
  virtual MeasureType Evaluate(MeasureType x);

  /** Gets the interval of each cell between the upper and lower bound */
  double GetInterval()
  { return m_TableInc; }

  /** y = f(x) = (approximately) cache_table(index(x))
   * Get the function return using the internal cache table
   * NOTE: Since the index calculation needs conversion from double
   * to int, truncation happens. As a result, the return values from
   * Evaluate(x) and GetCachedValue(x) may not be same for the same x. */
  inline MeasureType GetCachedValue(MeasureType x)
  {
    if ( x > m_CacheUpperBound || x < m_CacheLowerBound )
      {
      throw ExceptionObject(__FILE__, __LINE__);
      }
    // access table
    int index = (int)( ( x - m_CacheLowerBound ) / m_TableInc );
    return m_CacheTable[index];
  }

protected:
  /** Create the internal cache table and fill it with
   * pre-evaluated values. */
  void CreateCache(double lowerBound, double upperBound, SizeValueType sampleSize);

private:
  /** The number of samples will be precalcualted and saved in the
   * cache table. */
  SizeValueType m_NumberOfSamples;

  /** Storage for the precalcualted function values. */
  MeasureArrayType m_CacheTable;

  /** The upper-bound of domain that is used for filling the cache table. */
  double m_CacheUpperBound;

  /** The lower-bound of domain that is used for filling the cache table. */
  double m_CacheLowerBound;

  /** Sampling interval for function evaluation. */
  double m_TableInc;

  /** Is the cache available?   */
  bool m_CacheAvailable;
}; // end of class
} // end of namespace itk
#endif
