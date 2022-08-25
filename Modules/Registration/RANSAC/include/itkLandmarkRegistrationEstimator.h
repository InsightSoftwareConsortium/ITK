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

#ifndef itkLandmarkRegistrationEstimator_h
#define itkLandmarkRegistrationEstimator_h

#include "itkLandmarkRegistrationEstimator.h"
#include "itkPoint.h"
#include "itkObjectFactory.h"

namespace itk
{

template <unsigned int Dimension>
class ITK_TEMPLATE_EXPORT LandmarkRegistrationEstimator : public ParametersEstimator<Point<double, Dimension>, double>
{
public:
  typedef LandmarkRegistrationEstimator                         Self;
  typedef ParametersEstimator<Point<double, Dimension>, double> Superclass;
  typedef SmartPointer<Self>                                    Pointer;
  typedef SmartPointer<const Self>                              ConstPointer;

  itkTypeMacro(LandmarkRegistrationEstimator, ParametersEstimator);
  /** New method for creating an object using a factory. */
  itkNewMacro(Self);

  virtual void
  Estimate(std::vector<Point<double, Dimension> *> & data, std::vector<double> & parameters);
  virtual void
  Estimate(std::vector<Point<double, Dimension>> & data, std::vector<double> & parameters);

  virtual void
  LeastSquaresEstimate(std::vector<Point<double, Dimension> *> & data, std::vector<double> & parameters);
  virtual void
  LeastSquaresEstimate(std::vector<Point<double, Dimension>> & data, std::vector<double> & parameters);

  virtual bool
  Agree(std::vector<double> & parameters, Point<double, Dimension> & data);

  virtual std::vector<bool>
  AgreeMultiple(std::vector<double> & parameters, std::vector<Point<double, Dimension>> & data);

  virtual void
  SetDelta(double delta);

  virtual double
  GetDelta();

protected:
  LandmarkRegistrationEstimator();
  ~LandmarkRegistrationEstimator();

private:
  LandmarkRegistrationEstimator(const Self &); // purposely not implemented
  void
  operator=(const Self &); // purposely not implemented
                           // given line L and point P, if dist(L,P)^2 < delta^2 then the
                           // point is on the line
  double deltaSquared;
};

} // end namespace itk

// the implementation is in this file
#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkLandmarkRegistrationEstimator.hxx"
#endif

#endif //_PLANE_PARAM_ESTIMATOR_H_
