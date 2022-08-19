#ifndef itkLandmarkRegistrationEstimator_h
#define itkLandmarkRegistrationEstimator_h

#include "itkLandmarkRegistrationEstimator.h"
#include "itkPoint.h"
#include "itkObjectFactory.h"

namespace itk
{

template <unsigned int dimension>
class ITK_TEMPLATE_EXPORT LandmarkRegistrationEstimator : public ParametersEstimator<Point<double, dimension>, double>
{
public:
  typedef LandmarkRegistrationEstimator                         Self;
  typedef ParametersEstimator<Point<double, dimension>, double> Superclass;
  typedef SmartPointer<Self>                                    Pointer;
  typedef SmartPointer<const Self>                              ConstPointer;

  itkTypeMacro(LandmarkRegistrationEstimator, ParametersEstimator);
  /** New method for creating an object using a factory. */
  itkNewMacro(Self)

    virtual void Estimate(std::vector<Point<double, dimension> *> & data, std::vector<double> & parameters);
  virtual void
  Estimate(std::vector<Point<double, dimension>> & data, std::vector<double> & parameters);

  virtual void
  LeastSquaresEstimate(std::vector<Point<double, dimension> *> & data, std::vector<double> & parameters);
  virtual void
  LeastSquaresEstimate(std::vector<Point<double, dimension>> & data, std::vector<double> & parameters);

  virtual bool
  Agree(std::vector<double> & parameters, Point<double, dimension> & data);

  void
  SetDelta(double delta);

  double
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
#  include "itkLandmarkRegistrationEstimator.txx"
#endif

#endif //_PLANE_PARAM_ESTIMATOR_H_
