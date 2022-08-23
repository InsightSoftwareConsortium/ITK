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

#ifndef itkPlaneParametersEstimator_h
#define itkPlaneParametersEstimator_h

#include "itkParametersEstimator.h"
#include "itkPoint.h"
#include <itkObjectFactory.h>

namespace itk
{

/**
 * This class estimates the parameters of a (hyper)plane (2D line, 3D plane...).
 *
 * A (hyper)plane is represented as: (*) dot(n,p-a)=0
 *                                    where n is the (hyper)plane normal
 *                                    (|n| = 1) and 'a' is a point on the
 *                                    (hyper)plane.
 * All points 'p' which satisfy equation (*) are on the (hyper)plane.
 *
 * @author: Ziv Yaniv (zivy@isis.georgetown.edu)
 *
 */

template <unsigned int dimension>
class ITK_TEMPLATE_EXPORT PlaneParametersEstimator : public ParametersEstimator<Point<double, dimension>, double>
{
public:
  typedef PlaneParametersEstimator                              Self;
  typedef ParametersEstimator<Point<double, dimension>, double> Superclass;
  typedef SmartPointer<Self>                                    Pointer;
  typedef SmartPointer<const Self>                              ConstPointer;

  itkTypeMacro(PlaneParametersEstimator, ParametersEstimator);
  /** New method for creating an object using a factory. */
  itkNewMacro(Self)

    /**
     * Compute the (hyper)plane defined by the given data points.
     * @param data A vector containing k kD points.
     * @param parameters This vector is cleared and then filled with the computed
     *                   parameters. The parameters of the plane passing through
     *                   these points [n_0,...,n_k,a_0,...,a_k] where
     *                   ||(n_0,...,nk)|| = 1.
     *                   If the vector contains less than k points or the first k
     *                   points are linearly dependent then the resulting
     *                   parameters vector is empty (size = 0).
     */
    virtual void Estimate(std::vector<Point<double, dimension> *> & data, std::vector<double> & parameters);
  virtual void
  Estimate(std::vector<Point<double, dimension>> & data, std::vector<double> & parameters);

  /**
   * Compute a least squares estimate of the (hyper)plane defined by the given
   * points. This implementation is of an orthogonal least squares error.
   *
   * @param data The (hyper)plane should minimize the least squares error to
   *             these points.
   * @param parameters This vector is cleared and then filled with the computed
   *                   parameters. The parameters of the plane passing through
   *                   these points [n_0,...,n_k,a_0,...,a_k] where
   *                   ||(n_0,...,nk)|| = 1.
   *                   If the vector contains less than k, kD, linearly
   *                   independent points then the resulting parameters vector
   *                   is empty (size = 0).
   */
  virtual void
  LeastSquaresEstimate(std::vector<Point<double, dimension> *> & data, std::vector<double> & parameters);
  virtual void
  LeastSquaresEstimate(std::vector<Point<double, dimension>> & data, std::vector<double> & parameters);

  /**
   * Return true if the distance between the line defined by the parameters and
   * the
   * given point is smaller than 'delta' (see constructor).
   * @param parameters The line parameters [n_x,n_y,a_x,a_y].
   * @param data Check that the distance between this point and the line is
   *             less than 'delta'.
   */
  virtual bool
  Agree(std::vector<double> & parameters, Point<double, dimension> & data);

  /**
   * Set parameter which defines a threshold for a point to be considered on the
   * plane.
   * @param delta A point is on the (hyper)plane if its distance from the
   *              (hyper)plane is less than 'delta'.
   */
  void
  SetDelta(double delta);
  double
  GetDelta();

protected:
  PlaneParametersEstimator();
  ~PlaneParametersEstimator();

private:
  PlaneParametersEstimator(const Self &); // purposely not implemented
  void
  operator=(const Self &); // purposely not implemented
                           // given line L and point P, if dist(L,P)^2 < delta^2 then the
                           // point is on the line
  double deltaSquared;
};

} // end namespace itk

// the implementation is in this file
#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkPlaneParametersEstimator.hxx"
#endif

#endif //_PLANE_PARAM_ESTIMATOR_H_
