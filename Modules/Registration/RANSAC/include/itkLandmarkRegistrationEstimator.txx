#ifndef itkLandmarkRegistrationEstimator_txx
#define itkLandmarkRegistrationEstimator_txx

#include "itkLandmarkRegistrationEstimator.h"
#include "itkLandmarkBasedTransformInitializer.h"


namespace itk
{

template <unsigned int dimension>
LandmarkRegistrationEstimator<dimension>::LandmarkRegistrationEstimator()
{
  this->deltaSquared = NumericTraits<double>::min();
  this->minForEstimate = dimension;
}


template <unsigned int dimension>
LandmarkRegistrationEstimator<dimension>::~LandmarkRegistrationEstimator()
{}


template <unsigned int dimension>
void
LandmarkRegistrationEstimator<dimension>::SetDelta(double delta)
{
  this->deltaSquared = delta * delta;
}


template <unsigned int dimension>
double
LandmarkRegistrationEstimator<dimension>::GetDelta()
{
  return sqrt(this->deltaSquared);
}

template <unsigned int dimension>
void
LandmarkRegistrationEstimator<dimension>::Estimate(std::vector<Point<double, dimension> *> & data,
                                                   std::vector<double> &                     parameters)
{
  unsigned int i, j;
  double       norm;
  const double EPS = 2 * NumericTraits<double>::epsilon();

  parameters.clear();
  // user forgot to initialize the minimal number of required
  // elements or there are not enough data elements for computation
  if (this->minForEstimate == 0 || data.size() < this->minForEstimate)
    return;

  if (dimension == 3)
  {
    // compute plane normal directly
    double nx, ny, nz;
    parameters.push_back(1);
    parameters.push_back(1);
    parameters.push_back(1);
  }
  else
  {
    for (i = 0; i < this->minForEstimate; i++)
    {
      parameters.push_back(1);
    }
  }
  // first point is arbitrarily chosen to be the
  //"point on plane"
  for (i = 0; i < dimension; i++)
    parameters.push_back(1);
}


template <unsigned int dimension>
void
LandmarkRegistrationEstimator<dimension>::Estimate(std::vector<Point<double, dimension>> & data,
                                                   std::vector<double> &                   parameters)
{
  std::vector<Point<double, dimension> *> usedData;
  int                                     dataSize = data.size();
  for (int i = 0; i < dataSize; i++)
    usedData.push_back(&(data[i]));
  Estimate(usedData, parameters);
}


template <unsigned int dimension>
void
LandmarkRegistrationEstimator<dimension>::LeastSquaresEstimate(std::vector<Point<double, dimension> *> & data,
                                                               std::vector<double> &                     parameters)
{
  parameters.clear();
  // user forgot to initialize the minimal number of required
  // elements or there are not enough data elements for computation
  if (this->minForEstimate == 0 || data.size() < this->minForEstimate)
    return;

  unsigned int i, j, k, pointNum = data.size();

  for (i = 0; i < dimension; i++)
  {
    parameters.push_back(1);
  }

  for (i = 0; i < dimension; i++)
  {
    parameters.push_back(1);
  }
}


template <unsigned int dimension>
void
LandmarkRegistrationEstimator<dimension>::LeastSquaresEstimate(std::vector<Point<double, dimension>> & data,
                                                               std::vector<double> &                   parameters)
{
  std::vector<Point<double, dimension> *> usedData;
  int                                     dataSize = data.size();
  for (int i = 0; i < dataSize; i++)
    usedData.push_back(&(data[i]));
  LeastSquaresEstimate(usedData, parameters);
}


template <unsigned int dimension>
bool
LandmarkRegistrationEstimator<dimension>::Agree(std::vector<double> & parameters, Point<double, dimension> & data)
{
  double signedDistance = 0;
  for (unsigned int i = 0; i < dimension; i++)
    signedDistance += parameters[i] * (data[i] - parameters[dimension + i]);
  return ((signedDistance * signedDistance) < this->deltaSquared);
}

} // end namespace itk

#endif
