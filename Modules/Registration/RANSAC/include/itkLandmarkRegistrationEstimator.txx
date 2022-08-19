#ifndef itkLandmarkRegistrationEstimator_hxx
#define itkLandmarkRegistrationEstimator_hxx

#include "itkLandmarkRegistrationEstimator.h"
#include "itkLandmarkBasedTransformInitializer.h"
#include "itkSimilarity3DTransform.h"
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
  parameters.clear();
  // user forgot to initialize the minimal number of required
  // elements or there are not enough data elements for computation
  if (this->minForEstimate == 0 || data.size() < this->minForEstimate)
    return;
}

template <unsigned int dimension>
void
LandmarkRegistrationEstimator<dimension>::Estimate(std::vector<Point<double, dimension>> & data,
                                                   std::vector<double> &                   parameters)
{}

template <unsigned int dimension>
void
LandmarkRegistrationEstimator<dimension>::LeastSquaresEstimate(std::vector<Point<double, dimension> *> & data,
                                                               std::vector<double> &                     parameters)
{
  using PixelType = float;
  constexpr unsigned int Dimension = 3;
  using FixedImageType = itk::Image<PixelType, Dimension>;
  using MovingImageType = itk::Image<PixelType, Dimension>;

  using TransformType = itk::Similarity3DTransform<double>;
  using TransformInitializerType =
    itk::LandmarkBasedTransformInitializer<TransformType, FixedImageType, MovingImageType>;
  auto initializer = TransformInitializerType::New();

  typename TransformInitializerType::LandmarkPointContainer fixedLandmarks;
  typename TransformInitializerType::LandmarkPointContainer movingLandmarks;

  using Similarity3DTransformType = Similarity3DTransform<double>;

  parameters.clear();
  // user forgot to initialize the minimal number of required
  // elements or there are not enough data elements for computation
  if (this->minForEstimate == 0 || data.size() < this->minForEstimate)
    return;

  unsigned int pointNum = data.size();

  auto similarityTransform = Similarity3DTransformType::New();

  for (unsigned int i = 0; i < pointNum; ++i)
  {
  }


  for (unsigned int i = 0; i < dimension; i++)
  {
    parameters.push_back(1);
  }

  for (unsigned int i = 0; i < dimension; i++)
  {
    parameters.push_back(1);
  }
}

template <unsigned int dimension>
void
LandmarkRegistrationEstimator<dimension>::LeastSquaresEstimate(std::vector<Point<double, dimension>> & data,
                                                               std::vector<double> &                   parameters)
{}

template <unsigned int dimension>
bool
LandmarkRegistrationEstimator<dimension>::Agree(std::vector<double> & parameters, Point<double, dimension> & data)
{
  std::cout << "Pranjal Sahu Testing " << std::endl;
  return 1 < 0;
  // double signedDistance = 0;
  // for (unsigned int i = 0; i < dimension; i++)
  // {
  //   signedDistance += parameters[i] * (data[i] - parameters[dimension + i]);
  // }
  // return ((signedDistance * signedDistance) < this->deltaSquared);
}

} // end namespace itk

#endif
