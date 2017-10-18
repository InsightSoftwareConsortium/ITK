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
#ifndef itkCompositeValleyFunction_h
#define itkCompositeValleyFunction_h

#include "itkCacheableScalarFunction.h"
#include "ITKBiasCorrectionExport.h"
#include <vector>

namespace itk
{
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
 * function, higher-bound and lower-bound. Upper-bound will be highest mean
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
 * \ingroup ITKBiasCorrection
 */
class TargetClass
{
public:
  /** Constructor. */
  TargetClass(double mean, double sigma)
  {
    m_Mean = mean;
    m_Sigma = sigma;
  }

  /** Set/Get the mean of the function. */
  void SetMean(double mean) { m_Mean = mean; }
  double GetMean() { return m_Mean; }

  /** Set/Get the standard deviation of the function. */
  void SetSigma(double sigma) { m_Sigma = sigma; }
  double GetSigma() { return m_Sigma; }

private:
  double m_Mean;
  double m_Sigma;
}; // end of class

class ITKBiasCorrection_EXPORT CompositeValleyFunction:public CacheableScalarFunction
{
public:

  /** Superclass to this class. */
  typedef CacheableScalarFunction Superclass;

  /** Cost value type. */
  typedef  Superclass::MeasureType      MeasureType;
  typedef  Superclass::MeasureArrayType MeasureArrayType;

  /** Constructor. */
  CompositeValleyFunction(const MeasureArrayType & classMeans,
                          const MeasureArrayType & classSigmas);

  /** Destructor. */
    virtual ~CompositeValleyFunction() ITK_OVERRIDE;

  /** Get energy table's higher bound. */
  double GetUpperBound() { return m_UpperBound; }

  /** Get energy table's lower bound. */
  double GetLowerBound() { return m_LowerBound; }

  /** Gets an energy value for the intensity difference between a pixel
   * and its corresponding bias. */
  MeasureType operator()(MeasureType x)
  {
    if ( x > m_UpperBound || x < m_LowerBound )
      {
      return 1;
      }

    if ( !this->IsCacheAvailable() )
      {
      return this->Evaluate(x);
      }
    else
      {
      return GetCachedValue(x);
      }
  }

  /** Evalaute the function at point x.  */
  virtual MeasureType Evaluate(MeasureType x) ITK_OVERRIDE;

  /** Get an energy value for the valley. */
  inline MeasureType valley(MeasureType d)
  {
    return 1 - 1 / ( 1 + d * d / 3 );
  }

protected:
  void AddNewClass(double mean, double sigma)
  {
    TargetClass aClass(mean, sigma);

    m_Targets.push_back(aClass);
  }

  /** calculate and save energy values  */
  void Initialize();

private:
  /** Storage for tissue classes' statistics. */
  std::vector< TargetClass > m_Targets;

  /** The highest mean value + the sigma of the tissue class
   * which has the highest mean value * 9. */
  double m_UpperBound;

  /** The lowest mean value - the sigma of the tissue class
   * which has the lowest mean value * 9. */
  double m_LowerBound;
}; // end of class
} // end of namespace itk
#endif
