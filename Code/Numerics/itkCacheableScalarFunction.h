/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkCacheableScalarFunction.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

=========================================================================*/
#ifndef __itkCacheableScalarFunction_h
#define __itkCacheableScalarFunction_h

#include <vector>
#include <vnl/vnl_vector.h>

#include "itkExceptionObject.h"

namespace itk {
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
 */

class CacheableScalarFunction
{
public:
  
  /**
   * constructor:
   */
  CacheableScalarFunction() ;

  virtual ~CacheableScalarFunction() {}

  /**
   * function's input and output value type 
   */
  typedef double MeasureType ;

  /**
   * Get the number of samples between the lower-bound and higher-bound
   * of the cache table
   */
  long GetNumberOfSamples() { return m_NumberOfSamples ; } 

  /**
   * Check if the internal cache table and its values are valid
   */
  bool IsCacheAvailable() { return m_CacheAvailable ; } 
  
  /**
   * Get the upper-bound of domain that is used for filling the cache table
   */
  double GetCacheHigherBound() { return m_CacheHigherBound ; } 

  /**
   * Get the lower-bound of domain that is used for filling the cache table
   */
  double GetCacheLowerBound() { return m_CacheLowerBound ; } 

  /**
   * y = f(x)
   * Subclasses of this class should override this member function
   * to provide their own functional operation 
   */
  virtual MeasureType Evaluate(MeasureType x) 
  { return x ; }

  /**
   * y = f(x) = (approximately) cache_table(index(x))
   * Get the function return using the internal cache table
   * NOTE: Since the index calculation needs conversion from double
   * to int, truncation happens. As a result, the return values from
   * Evaluate(x) and GetCachedValue(x) may not be same for the same x
   */
  inline MeasureType GetCachedValue(MeasureType x)
  {
    if (x > m_CacheHigherBound || x < m_CacheLowerBound)
      {
        throw ExceptionObject(__FILE__, __LINE__) ;
      }

    // access table
    int index = (int) ((x - m_CacheLowerBound) / m_TableInc + 0.5) ;
    return (*m_CacheTable)[index] ;
  }

protected:

  /**
   * Create the internal cache table and fill it with 
   * pre-evaluated values
   */
  void CreateCache(double lowerBound,
                   double higherBound,
                   long sampleSize) ;

  
private:
  /**
   * the number of samples will be precalcualted and saved in the
   * cache table
   */
  long m_NumberOfSamples ;

  /**
   * storage for the precalcualted function values
   */
  vnl_vector<MeasureType>* m_CacheTable ;

  /**
   * the upper-bound of domain that is used for filling the cache table
   */
  double m_CacheHigherBound ;

  /**
   * the lower-bound of domain that is used for filling the cache table
   */
  double m_CacheLowerBound ;

  /**
   * sampling interval for function evaluation
   */
  double m_TableInc ;

  /**
   *
   */
  bool m_CacheAvailable ;
} ; // end of class
} // end of namespace itk
#endif
