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

#ifndef itkLandmarkRegistrationEstimator_hxx
#define itkLandmarkRegistrationEstimator_hxx

#include "itkLandmarkRegistrationEstimator.h"
#include "itkLandmarkBasedTransformInitializer.h"
#include "itkIntTypes.h"
#include "nanoflann.hpp"
#include "itkMesh.h"
#include "itkTransformMeshFilter.h"

namespace itk
{


template <unsigned int Dimension, typename TTransform>
LandmarkRegistrationEstimator<Dimension, TTransform>::LandmarkRegistrationEstimator()
{
  this->delta = NumericTraits<double>::min();
  this->minForEstimate = Dimension;
}

template <unsigned int Dimension, typename TTransform>
void
LandmarkRegistrationEstimator<Dimension, TTransform>::SetDelta(double inputDelta)
{
  this->delta = inputDelta;
}

template <unsigned int Dimension, typename TTransform>
double
LandmarkRegistrationEstimator<Dimension, TTransform>::GetDelta()
{
  return this->delta;
}


template <unsigned int Dimension, typename TTransform>
void
LandmarkRegistrationEstimator<Dimension, TTransform>::Estimate(std::vector<Point<double, Dimension> *> & data,
                                                               std::vector<double> &                     parameters)
{
  parameters.clear();

  constexpr unsigned int DimensionPoint = 3;
  using PixelType = float;
  using FixedImageType = itk::Image<PixelType, DimensionPoint>;
  using MovingImageType = itk::Image<PixelType, DimensionPoint>;

  using TransformInitializerType = itk::LandmarkBasedTransformInitializer<TTransform, FixedImageType, MovingImageType>;
  auto initializer = TransformInitializerType::New();
  auto transform = TTransform::New();

  typename TransformInitializerType::LandmarkPointContainer fixedLandmarks;
  typename TransformInitializerType::LandmarkPointContainer movingLandmarks;

  fixedLandmarks.reserve(data.size());
  movingLandmarks.reserve(data.size());

  // Create landmark points from the 6D input points
  for (unsigned int i = 0; i < data.size(); ++i)
  {
    Point<double, Dimension> & pnt = *(data[i]);
    itk::Point<double, 3>      point1;
    point1[0] = pnt[0];
    point1[1] = pnt[1];
    point1[2] = pnt[2];
    fixedLandmarks.push_back(point1);

    itk::Point<double, 3> point2;
    point2[0] = pnt[3];
    point2[1] = pnt[4];
    point2[2] = pnt[5];
    movingLandmarks.push_back(point2);
  }

  initializer->SetMovingLandmarks(movingLandmarks);
  initializer->SetFixedLandmarks(fixedLandmarks);
  initializer->SetTransform(transform);
  initializer->InitializeTransform();

  // Copy the transform parameters in the input variable
  auto transformParameters = transform->GetParameters();
  for (unsigned int i = 0; i < transformParameters.GetSize(); ++i)
  {
    parameters.push_back(transformParameters.GetElement(i));
  }

  auto fixedParameters = transform->GetFixedParameters();
  for (unsigned int i = 0; i < fixedParameters.GetSize(); ++i)
  {
    parameters.push_back(fixedParameters.GetElement(i));
  }
  return;
}

template <unsigned int Dimension, typename TTransform>
void
LandmarkRegistrationEstimator<Dimension, TTransform>::Estimate(std::vector<Point<double, Dimension>> & data,
                                                               std::vector<double> &                   parameters)
{
  std::vector<Point<double, Dimension> *> usedData;
  int                                     dataSize = data.size();
  for (int i = 0; i < dataSize; i++)
  {
    usedData.push_back(&(data[i]));
  }
  Estimate(usedData, parameters);
}

template <unsigned int Dimension, typename TTransform>
void
LandmarkRegistrationEstimator<Dimension, TTransform>::LeastSquaresEstimate(
  std::vector<Point<double, Dimension> *> & data,
  std::vector<double> &                     parameters)
{
  parameters.clear();

  using PixelType = float;
  constexpr unsigned int DimensionPoint = 3;
  using FixedImageType = itk::Image<PixelType, DimensionPoint>;
  using MovingImageType = itk::Image<PixelType, DimensionPoint>;

  using TransformInitializerType = itk::LandmarkBasedTransformInitializer<TTransform, FixedImageType, MovingImageType>;

  auto initializer = TransformInitializerType::New();
  auto transform = TTransform::New();

  itk::Point<double, 3>                                     point;
  typename TransformInitializerType::LandmarkPointContainer fixedLandmarks;
  typename TransformInitializerType::LandmarkPointContainer movingLandmarks;

  // Create landmark points from the 6D input points
  for (unsigned int i = 0; i < data.size(); ++i)
  {
    Point<double, Dimension> & pnt = *(data[i]);

    point[0] = pnt[0];
    point[1] = pnt[1];
    point[2] = pnt[2];
    fixedLandmarks.push_back(point);

    point[0] = pnt[3];
    point[1] = pnt[4];
    point[2] = pnt[5];
    movingLandmarks.push_back(point);
  }

  initializer->SetMovingLandmarks(movingLandmarks);
  initializer->SetFixedLandmarks(fixedLandmarks);
  initializer->SetTransform(transform);
  initializer->InitializeTransform();

  // Copy the transform parameters in the input variable
  auto transformParameters = transform->GetParameters();
  for (unsigned int i = 0; i < transformParameters.GetSize(); ++i)
  {
    parameters.push_back(transformParameters.GetElement(i));
  }

  auto fixedParameters = transform->GetFixedParameters();
  for (unsigned int i = 0; i < fixedParameters.GetSize(); ++i)
  {
    parameters.push_back(fixedParameters.GetElement(i));
  }
  return;
}

template <unsigned int Dimension, typename TTransform>
void
LandmarkRegistrationEstimator<Dimension, TTransform>::LeastSquaresEstimate(std::vector<Point<double, Dimension>> & data,
                                                                           std::vector<double> & parameters)
{
  std::vector<Point<double, Dimension> *> usedData;
  int                                     dataSize = data.size();
  usedData.reserve(dataSize);
  for (int i = 0; i < dataSize; i++)
  {
    usedData.push_back(&(data[i]));
  }
  LeastSquaresEstimate(usedData, parameters);
}

template <unsigned int Dimension, typename TTransform>
bool
LandmarkRegistrationEstimator<Dimension, TTransform>::Agree(std::vector<double> &      parameters,
                                                            Point<double, Dimension> & data)
{
  auto transform = TTransform::New();

  auto optParameters = transform->GetParameters();
  auto fixedParameters = transform->GetFixedParameters();

  int          counter = 0;
  unsigned int totalParameters = optParameters.GetSize() + fixedParameters.GetSize();
  for (unsigned int i = optParameters.GetSize(); i < totalParameters; ++i)
  {
    fixedParameters.SetElement(counter, parameters[i]);
    counter = counter + 1;
  }
  transform->SetFixedParameters(fixedParameters);

  counter = 0;
  for (unsigned int i = 0; i < optParameters.GetSize(); ++i)
  {
    optParameters.SetElement(counter, parameters[i]);
    counter = counter + 1;
  }
  transform->SetParameters(optParameters);

  itk::Point<double, 3> p0;
  itk::Point<double, 3> p1;

  p0[0] = data[0];
  p0[1] = data[1];
  p0[2] = data[2];

  p1[0] = data[3];
  p1[1] = data[4];
  p1[2] = data[5];

  auto transformedPoint = transform->TransformPoint(p0);
  auto distance = transformedPoint.EuclideanDistanceTo(p1);
  return (distance < this->delta);
}


template <unsigned int Dimension, typename TTransform>
void
LandmarkRegistrationEstimator<Dimension, TTransform>::SetAgreeData(std::vector<Point<double, Dimension>> & data)
{
  this->agreePoints = PointsContainer::New();
  this->agreePoints->reserve(data.size());

  this->samples.resize(data.size());

  unsigned int          dim = 3;
  itk::Point<double, 3> testPoint;

  for (unsigned int i = 0; i < data.size(); ++i)
  {
    auto point = data[i];
    testPoint[0] = point[3];
    testPoint[1] = point[4];
    testPoint[2] = point[5];
    this->agreePoints->InsertElement(i, testPoint);

    this->samples[i].resize(dim);
    for (size_t d = 0; d < dim; d++)
    {
      this->samples[i][d] = testPoint[d];
    }
  }

  this->mat_adaptor = new KdTreeT(dim, this->samples, 5);
  this->mat_adaptor->index->buildIndex();
}

template <unsigned int Dimension, typename TTransform>
bool
LandmarkRegistrationEstimator<Dimension, TTransform>::CheckCorresspondenceEdgeLength(
  std::vector<double> &                     parameters,
  std::vector<Point<double, Dimension> *> & data,
  double                                    inputEdgeLength)
{
  itk::Point<double, 3> p0_source;
  itk::Point<double, 3> p1_source;

  itk::Point<double, 3> p0_dest;
  itk::Point<double, 3> p1_dest;

  double similarity_threshold = inputEdgeLength;

  unsigned int dataSize = data.size();
  for (unsigned int i = 0; i < dataSize; ++i)
  {
    for (unsigned int j = i + 1; j < dataSize; ++j)
    {
      Point<double, Dimension> & temp_point1 = *(data[i]);
      Point<double, Dimension> & temp_point2 = *(data[j]);

      for (unsigned int k = 0; k < 3; ++k)
      {
        p0_source[k] = temp_point1[k];
        p0_dest[k] = temp_point1[k + 3];

        p1_source[k] = temp_point2[k];
        p1_dest[k] = temp_point2[k + 3];
      }

      auto dis_source = p0_source.EuclideanDistanceTo(p1_source);
      auto dis_target = p0_dest.EuclideanDistanceTo(p1_dest);

      if (dis_source < dis_target * similarity_threshold || dis_target < dis_source * similarity_threshold)
      {
        return false;
      }
    }
  }

  return true;
}

template <unsigned int Dimension, typename TTransform>
bool
LandmarkRegistrationEstimator<Dimension, TTransform>::CheckCorresspondenceDistance(
  std::vector<double> &                     parameters,
  std::vector<Point<double, Dimension> *> & data)
{
  auto transform = TTransform::New();

  auto optParameters = transform->GetParameters();
  auto fixedParameters = transform->GetFixedParameters();

  int          counter = 0;
  unsigned int totalParameters = optParameters.GetSize() + fixedParameters.GetSize();
  for (unsigned int i = optParameters.GetSize(); i < totalParameters; ++i)
  {
    fixedParameters.SetElement(counter, parameters[i]);
    counter = counter + 1;
  }
  transform->SetFixedParameters(fixedParameters);

  counter = 0;
  for (unsigned int i = 0; i < optParameters.GetSize(); ++i)
  {
    optParameters.SetElement(counter, parameters[i]);
    counter = counter + 1;
  }
  transform->SetParameters(optParameters);

  itk::Point<double, 3> p0;
  itk::Point<double, 3> p1;

  unsigned int dataSize = data.size();
  for (unsigned int i = 0; i < dataSize; ++i)
  {
    Point<double, Dimension> & pnt = *(data[i]);

    p0[0] = pnt[0];
    p0[1] = pnt[1];
    p0[2] = pnt[2];

    p1[0] = pnt[3];
    p1[1] = pnt[4];
    p1[2] = pnt[5];

    auto transformedPoint = transform->TransformPoint(p0);
    auto distance = p1.EuclideanDistanceTo(transformedPoint);
    if (distance > this->delta)
    {
      return false;
    }
  }

  return true;
}

template <unsigned int Dimension, typename TTransform>
std::vector<double>
LandmarkRegistrationEstimator<Dimension, TTransform>::AgreeMultiple(std::vector<double> &                   parameters,
                                                                    std::vector<Point<double, Dimension>> & data,
                                                                    unsigned int                            currentBest)
{
  auto transform = TTransform::New();

  auto optParameters = transform->GetParameters();
  auto fixedParameters = transform->GetFixedParameters();

  int          counter = 0;
  unsigned int totalParameters = optParameters.GetSize() + fixedParameters.GetSize();
  for (unsigned int i = optParameters.GetSize(); i < totalParameters; ++i)
  {
    fixedParameters.SetElement(counter, parameters[i]);
    counter = counter + 1;
  }
  transform->SetFixedParameters(fixedParameters);

  counter = 0;
  for (unsigned int i = 0; i < optParameters.GetSize(); ++i)
  {
    optParameters.SetElement(counter, parameters[i]);
    counter = counter + 1;
  }
  transform->SetParameters(optParameters);

  std::vector<double> query_pt(3);
  std::vector<double> output(data.size());
  // output.reserve(data.size());

  const size_t                    num_results = 1;
  std::vector<size_t>             ret_indexes(num_results);
  std::vector<double>             out_dists_sqr(num_results);
  nanoflann::KNNResultSet<double> resultSet(num_results);

  itk::Point<double, 3> p0;

  unsigned int localBest = 0;
  unsigned int dataSize = data.size();

  for (unsigned int i = 0; i < dataSize; ++i)
  {
    // For early stopping. No point running if this condition is true
    if (localBest + dataSize - i < currentBest)
    {
      break;
    }
    p0[0] = data[i][0];
    p0[1] = data[i][1];
    p0[2] = data[i][2];

    auto transformedPoint = transform->TransformPoint(p0);

    query_pt[0] = transformedPoint[0];
    query_pt[1] = transformedPoint[1];
    query_pt[2] = transformedPoint[2];

    resultSet.init(&ret_indexes[0], &out_dists_sqr[0]);
    this->mat_adaptor->index->findNeighbors(resultSet, &query_pt[0], nanoflann::SearchParams(10));
    bool flag = out_dists_sqr[0] < this->delta;
    if (flag)
    {
      localBest++;
      output[i] = out_dists_sqr[0];
    }
    else
    {
      output[i] = -1;
    }
  }

  return output;
}


} // end namespace itk

#endif
