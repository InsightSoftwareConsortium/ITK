/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkCompositeValleyFunction.h
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
#ifndef __itkCompositeValleyFunction_h
#define __itkCompositeValleyFunction_h

#include <vector>
#include <vnl/vnl_vector.h>

#include "itkCacheableScalarFunction.h"

namespace itk {

/** \class CompositeValleyFunction
 * \brief Multiple valley shaped curve function   
 *
 * Its functional form f(x) is :
 * sum (valley( (x - mean[i]) / sigma[i] ) ) 
 * over i from 0 to the number of target classes
 * where valley(x) = 1 - 1 / (1 + x^2 / 3) 
 * 
 * The plotting of the function return shows multiple lowest points at each
 * mean[i] position. There are two more important shape parameters for this
 * function, higher-bound and lower-bound. Higher-bound will be highest mean
 * value among target classes' means + its sigma value * 9, and lower-bound
 * will be lowest mean value among target classes' means - its sigma value * 9
 * 
 * For example, if there are two target classes with their means at 4 and 6.
 * The plotting may look like the following:
 *
 *    |
 *    |*********               ******
 *    |         *             *
 *    |          *    *      *  
 *    |           *  *  *   *
 *    |           * *    * *
 *    |           * *    * *
 *    |            *      *
 * ---+-----+------*------*-------
 *    |     2      4      6
 *    |
 * 
 *
 * This is a part of the bias correction methods and implementaion that
 * was initially developed and implemented 
 * by Martin Styner, Univ. of North Carolina at Chapel Hill, and his
 * colleagues.
 *
 * For more details. refer to the following articles.
 * "Parametric estimate of intensity inhomogeneities applied to MRI" 
 * Martin Styner, G. Gerig, Christian Brechbuehler, Gabor Szekely,  
 * IEEE TRANSACTIONS ON MEDICAL IMAGING; 19(3), pp. 153-165, 2000, 
 * (http://www.cs.unc.edu/~styner/docs/tmi00.pdf)
 *
 * "Evaluation of 2D/3D bias correction with 1+1ES-optimization" 
 * Martin Styner, Prof. Dr. G. Gerig (IKT, BIWI, ETH Zuerich), TR-197
 * (http://www.cs.unc.edu/~styner/docs/StynerTR97.pdf)
 */
class TargetClass
{
public:
  /** Constructor. */
  TargetClass(double mean, double sigma) 
    { 
    m_Mean = mean ;
    m_Sigma = sigma ;
    }
  
  /** Set/Get the mean of the function. */
  void SetMean(double mean) { m_Mean = mean ; }  
  double GetMean() { return m_Mean ; } 
  
  /** Set/Get the standard deviation of the function. */
  void SetSigma(double sigma) { m_Sigma = sigma ; }
  double GetSigma() { return m_Sigma ; }
    
private:
  double m_Mean ;
  double m_Sigma ;
} ; // end of class 

class CompositeValleyFunction : public CacheableScalarFunction
{
public:
  /** Cost value type. */
  typedef double MeasureType ;

  /** Superclass to this class. */
  typedef CacheableScalarFunction Superclass ;

  /** Constructor. */
  CompositeValleyFunction(std::vector<double>& classMeans, 
                                std::vector<double>& classSigmas) ;

  /** Destructor. */
  virtual ~CompositeValleyFunction() {}

  /** Get energy table's higher bound. */
  double GetHigherBound() { return m_HigherBound ; }

  /** Get energy table's lower bound. */
  double GetLowerBound() { return m_LowerBound ; }

  /** Gets an energy value for the intensity difference between a pixel
   * and its corresponding bias. */
  MeasureType operator() (MeasureType x)
  {
    if (x > m_HigherBound || x < m_LowerBound) { return 1; }

    if (!this->IsCacheAvailable()) 
      { return this->Evaluate(x); } 
    else 
      { return GetCachedValue(x); }
  }

  /** Get an energy value for the valley. */
  inline MeasureType valley(MeasureType d) 
    { return 1 - 1 / (1+d*d/3); }

protected:
  void AddNewClass(double mean, double sigma)
  {
    TargetClass aClass(mean, sigma) ;
    m_Targets.push_back(aClass) ;
  }

  /** calculate and save energy values  */
  void Initialize() ;

  /** Evalaute the function at point x.  */
  inline MeasureType Evaluate(MeasureType x) 
  {
    MeasureType res = 1;
    
    for (unsigned int k = 0 ; k < m_Targets.size() ; k++)
      {
      res *= valley( ( x - m_Targets[k].GetMean() ) /
                     m_Targets[k].GetSigma() );
      }
    
    return res;
  }  

private:
  /** Storage for tissue classes' statistics. */
  std::vector<TargetClass> m_Targets ;

  /** The highest mean value + the sigma of the tissue class 
   * which has the highest mean value * 9. */
  double m_HigherBound ;

  /** The lowest mean value - the sigma of the tissue class 
   * which has the lowest mean value * 9. */
  double m_LowerBound ;

} ; // end of class

} // end of namespace itk
#endif
