#ifndef itkLandmarkRegistrationEstimator_hxx
#define itkLandmarkRegistrationEstimator_hxx

#include "itkLandmarkRegistrationEstimator.h"
#include "itkLandmarkBasedTransformInitializer.h"
#include "itkSimilarity3DTransform.h"

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
{
  std::cout << "Pranjal Sahu Testing " << data.size() << parameters.size() << std::endl;
  return;
}

template <unsigned int dimension>
void
LandmarkRegistrationEstimator<dimension>::LeastSquaresEstimate(std::vector<Point<double, dimension> *> & data,
                                                               std::vector<double> &                     parameters)
{
  std::cout << "Pranjal Sahu Testing " << data.size() << parameters.size() << std::endl;
  return;
}

template <unsigned int dimension>
void
LandmarkRegistrationEstimator<dimension>::LeastSquaresEstimate(std::vector<Point<double, dimension>> & data,
                                                               std::vector<double> &                   parameters)
{
  using PixelType = float;
  constexpr unsigned int Dimension = 3;
  using FixedImageType = itk::Image<PixelType, Dimension>;
  using MovingImageType = itk::Image<PixelType, Dimension>;

  using TransformType = itk::Similarity3DTransform<double>;
  using TransformInitializerType =
    itk::LandmarkBasedTransformInitializer<TransformType, FixedImageType, MovingImageType>;
  auto initializer = TransformInitializerType::New();

  itk::Point<double, 3>                                     point;
  typename TransformInitializerType::LandmarkPointContainer fixedLandmarks;
  typename TransformInitializerType::LandmarkPointContainer movingLandmarks;

  // Create landmark points from the 6D input points
  for (unsigned int i = 0; i < data.size(); ++i)
  {
    point[0] = data[i][0];
    point[1] = data[i][1];
    point[2] = data[i][2];
    fixedLandmarks.push_back(point);

    point[0] = data[i][3];
    point[1] = data[i][4];
    point[2] = data[i][5];
    movingLandmarks.push_back(point);
  }

  // Obtain the parameters of the Similarity3DTransform
  using Similarity3DTransformType = Similarity3DTransform<double>;
  auto transform = Similarity3DTransformType::New();

  initializer->SetMovingLandmarks(movingLandmarks);
  initializer->SetFixedLandmarks(fixedLandmarks);
  initializer->SetTransform(transform);
  initializer->InitializeTransform();

  // Copy the transform parameters in the input variable
  parameters.clear();
  auto transformParameters = transform->GetParameters();
  for (unsigned int i = 0; i < transformParameters.Size(); ++i)
  {
    parameters.push_back(transformParameters.GetElement(i));
  }
  return;
}

template <unsigned int dimension>
bool
LandmarkRegistrationEstimator<dimension>::Agree(std::vector<double> & parameters, Point<double, dimension> & data)
{
  std::cout << "Pranjal Sahu Testing " << data.size() << parameters.size() << std::endl;
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
