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

#ifndef itkParametersEstimator_h
#define itkParametersEstimator_h

#include <vector>
#include "itkObject.h"


namespace itk
{

/**
 * This class defines the interface for parameter estimators.
 * Classes which inherit from it can be used by the RANSAC class to perform
 * robust parameter estimation.
 * The interface includes three methods:
 *     1.Estimate() - Estimation of the parameters using the minimal amount of
 *                    data (exact estimate).
 *     2.LeastSquaresEstimate() - Estimation of the parameters using
 *                                overdetermined data, so that the estimate
 *                                minimizes a least squres error criteria.
 *     3.Agree() - Does the given data agree with the model parameters.
 *
 * Template parameters:
 *    T - data type (e.g. itk::Point).
 *    S - parameter's type (e.g. double).
 *
 * @author: Ziv Yaniv zivy@isis.georgetown.edu
 */

template <typename T, typename SType>
class ITK_TEMPLATE_EXPORT ParametersEstimator : public Object
{
public:
  typedef ParametersEstimator      Self;
  typedef Object                   Superclass;
  typedef SmartPointer<Self>       Pointer;
  typedef SmartPointer<const Self> ConstPointer;

  itkTypeMacro(ParametersEstimator, Object);

  /**
   * Exact estimation of parameters.
   * @param data The data used for the estimate.
   * @param parameters This vector is cleared and then filled with the estimated
   *                   parameter values.
   */
  virtual void
  Estimate(std::vector<T *> & data, std::vector<SType> & parameters) = 0;
  virtual void
  Estimate(std::vector<T> & data, std::vector<SType> & parameters) = 0;

  /**
   * Least squares estimation of parameters.
   * @param data The data used for the estimate.
   * @param parameters This vector is cleared and then filled with the computed
   *                   parameters.
   */
  virtual void
  LeastSquaresEstimate(std::vector<T *> & data, std::vector<SType> & parameters) = 0;
  virtual void
  LeastSquaresEstimate(std::vector<T> & data, std::vector<SType> & parameters) = 0;

  /**
   * This method tests if the given data agrees with the model defined by the
   * parameters.
   */
  virtual bool
  Agree(std::vector<SType> & parameters, T & data) = 0;

  /**
   * Set the minimal number of data objects required for computation of an exact
   * estimate.
   * @param minForEstimate Minimal number of data objects required for
   *                       computation of an exact estimate.
   */
  void
  SetMinimalForEstimate(unsigned int minForEstimate);
  unsigned int
  GetMinimalForEstimate();

protected:
  ParametersEstimator() { this->minForEstimate = 0; }
  ~ParametersEstimator() {}

  // minimal number of data objects required for an exact estimate
  unsigned int minForEstimate;

private:
  ParametersEstimator(const Self &); // purposely not implemented
  void
  operator=(const Self &); // purposely not implemented
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkParametersEstimator.hxx"
#endif

#endif //_PARAMETERS_ESTIMATOR_H_
