/*=========================================================================
 *
 *  Copyright NumFOCUS
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

#include "itkLandmarkBasedTransformInitializer.h"
#include "itkImage.h"
#include "itkObject.h"
#include "itkTestingMacros.h"

#include <iostream>

template <unsigned int Dimension>
typename itk::Image<unsigned char, Dimension>::Pointer
CreateTestImage()
{
  using FixedImageType = itk::Image<unsigned char, Dimension>;
  typename FixedImageType::Pointer image = FixedImageType::New();

  typename FixedImageType::RegionType fRegion;
  typename FixedImageType::SizeType   fSize;
  typename FixedImageType::IndexType  fIndex;
  fSize.Fill(30); // size 30 x 30 x 30
  fIndex.Fill(0);
  fRegion.SetSize(fSize);
  fRegion.SetIndex(fIndex);
  image->SetLargestPossibleRegion(fRegion);
  image->SetBufferedRegion(fRegion);
  image->SetRequestedRegion(fRegion);
  image->Allocate();
  return image;
}

// Moving Landmarks = Fixed Landmarks rotated by 'angle' degrees and then
// translated by the 'translation'. Offset can be used to move the fixed
// landmarks around.
template <typename TTransformInitializer>
void
Init3DPoints(typename TTransformInitializer::LandmarkPointContainer & fixedLandmarks,
             typename TTransformInitializer::LandmarkPointContainer & movingLandmarks)
{
  const double nPI = 4.0 * std::atan(1.0);

  typename TTransformInitializer::LandmarkPointType point;
  typename TTransformInitializer::LandmarkPointType tmp;

  double angle = 10 * nPI / 180.0;

  typename TTransformInitializer::LandmarkPointType translation;
  translation[0] = 6;
  translation[1] = 10;
  translation[2] = 7;

  typename TTransformInitializer::LandmarkPointType offset;
  offset[0] = 10;
  offset[1] = 1;
  offset[2] = 5;
  point[0] = 2 + offset[0];
  point[1] = 2 + offset[1];
  point[2] = 0 + offset[2];
  fixedLandmarks.push_back(point);

  tmp = point;

  point[0] = std::cos(angle) * point[0] - std::sin(angle) * point[1] + translation[0];
  point[1] = std::sin(angle) * tmp[0] + std::cos(angle) * point[1] + translation[1];
  point[2] = point[2] + translation[2];
  movingLandmarks.push_back(point);

  point[0] = 2 + offset[0];
  point[1] = -2 + offset[1];
  point[2] = 0 + offset[2];
  fixedLandmarks.push_back(point);

  tmp = point;

  point[0] = std::cos(angle) * point[0] - std::sin(angle) * point[1] + translation[0];
  point[1] = std::sin(angle) * tmp[0] + std::cos(angle) * point[1] + translation[1];
  point[2] = point[2] + translation[2];
  movingLandmarks.push_back(point);

  point[0] = -2 + offset[0];
  point[1] = 2 + offset[1];
  point[2] = 0 + offset[2];
  fixedLandmarks.push_back(point);

  tmp = point;

  point[0] = std::cos(angle) * point[0] - std::sin(angle) * point[1] + translation[0];
  point[1] = std::sin(angle) * tmp[0] + std::cos(angle) * point[1] + translation[1];
  point[2] = point[2] + translation[2];
  movingLandmarks.push_back(point);

  point[0] = -2 + offset[0];
  point[1] = -2 + offset[1];
  point[2] = 0 + offset[2];
  fixedLandmarks.push_back(point);

  tmp = point;

  point[0] = std::cos(angle) * point[0] - std::sin(angle) * point[1] + translation[0];
  point[1] = std::sin(angle) * tmp[0] + std::cos(angle) * point[1] + translation[1];
  point[2] = point[2] + translation[2];

  movingLandmarks.push_back(point);
}

template <typename TransformInitializerType>
bool
ExecuteAndExamine(typename TransformInitializerType::Pointer                initializer,
                  typename TransformInitializerType::LandmarkPointContainer fixedLandmarks,
                  typename TransformInitializerType::LandmarkPointContainer movingLandmarks,
                  unsigned                                                  failLimit = 0)
{
  typename TransformInitializerType::TransformType::Pointer transform = TransformInitializerType::TransformType::New();
  initializer->SetTransform(transform);
  initializer->InitializeTransform();

  // Transform the landmarks now. For the given set of landmarks, since we computed the
  // moving landmarks explicitly from the rotation and translation specified, we should
  // get a transform that does not give any mismatch. In other words, if the fixed
  // landmarks are transformed by the transform computed by the
  // LandmarkBasedTransformInitializer, they should coincide exactly with the moving
  // landmarks. Note that we specified 4 landmarks, although three non-collinear
  // landmarks is sufficient to guarantee a solution.
  typename TransformInitializerType::PointsContainerConstIterator fitr = fixedLandmarks.begin();
  typename TransformInitializerType::PointsContainerConstIterator mitr = movingLandmarks.begin();

  using OutputVectorType = typename TransformInitializerType::OutputVectorType;
  OutputVectorType                         error;
  typename OutputVectorType::RealValueType tolerance = 0.1;
  unsigned int                             failed = 0;

  while (mitr != movingLandmarks.end())
  {
    typename TransformInitializerType::LandmarkPointType transformedPoint = transform->TransformPoint(*fitr);
    std::cout << " Fixed Landmark: " << *fitr << " Moving landmark " << *mitr
              << " Transformed fixed Landmark : " << transformedPoint;

    error = *mitr - transformedPoint;
    std::cout << " error = " << error.GetNorm() << std::endl;
    if (error.GetNorm() > tolerance)
    {
      failed++;
    }

    ++mitr;
    ++fitr;
  }

  if (failed > failLimit)
  {
    std::cout << " Fixed landmarks transformed by the transform did not match closely "
              << "enough with the moving landmarks.  The transform computed was: ";
    transform->Print(std::cout);
    std::cout << "[FAILED]" << std::endl;
    return false;
  }
  else
  {
    std::cout << " Landmark alignment using " << transform->GetNameOfClass() << " [PASSED]" << std::endl;
  }
  return true;
}

// Test LandmarkBasedTransformInitializer for given transform type
// Returns false if test failed, true if it succeeded
template <typename TransformType>
bool
test1()
{
  typename TransformType::Pointer transform = TransformType::New();
  std::cout << "Testing Landmark alignment with " << transform->GetNameOfClass() << std::endl;

  using PixelType = unsigned char;
  constexpr unsigned int Dimension = 3;
  using FixedImageType = itk::Image<PixelType, Dimension>;
  using MovingImageType = itk::Image<PixelType, Dimension>;

  using TransformInitializerType =
    itk::LandmarkBasedTransformInitializer<TransformType, FixedImageType, MovingImageType>;
  typename TransformInitializerType::Pointer initializer = TransformInitializerType::New();

  typename TransformInitializerType::LandmarkPointContainer fixedLandmarks;
  typename TransformInitializerType::LandmarkPointContainer movingLandmarks;
  Init3DPoints<TransformInitializerType>(fixedLandmarks, movingLandmarks);

  // No landmarks are set, it should throw
  ITK_TRY_EXPECT_EXCEPTION(initializer->InitializeTransform());
  initializer->SetMovingLandmarks(movingLandmarks);
  ITK_TRY_EXPECT_EXCEPTION(initializer->InitializeTransform()); // Fixed landmarks missing
  initializer->SetFixedLandmarks(fixedLandmarks);

  return ExecuteAndExamine<TransformInitializerType>(initializer, fixedLandmarks, movingLandmarks);
}

// The test specifies a bunch of fixed and moving landmarks and tests if the
// fixed landmarks after transform by the computed transform coincides
// with the moving landmarks.
int
itkLandmarkBasedTransformInitializerTest(int, char *[])
{
  bool success = true;
  success &= test1<itk::VersorRigid3DTransform<double>>();
  // Rigid3DTransform isn't supported by the landmark based initializer
  // success &= test1<itk::Rigid3DTransform< double > >();

  using PixelType = unsigned char;

  {
    // Test landmark alignment using Rigid 2D transform in 2 dimensions
    std::cout << "Testing Landmark alignment with Rigid2DTransform" << std::endl;
    constexpr unsigned int Dimension = 2;
    using FixedImageType = itk::Image<PixelType, Dimension>;
    using MovingImageType = itk::Image<PixelType, Dimension>;

    FixedImageType::Pointer  fixedImage = CreateTestImage<Dimension>();
    MovingImageType::Pointer movingImage = CreateTestImage<Dimension>();

    // Set the transform type
    using TransformType = itk::Rigid2DTransform<double>;
    using TransformInitializerType =
      itk::LandmarkBasedTransformInitializer<TransformType, FixedImageType, MovingImageType>;
    TransformInitializerType::Pointer initializer = TransformInitializerType::New();

    ITK_EXERCISE_BASIC_OBJECT_METHODS(initializer, LandmarkBasedTransformInitializer, Object);

    initializer->DebugOn();

    // Set fixed and moving landmarks
    TransformInitializerType::LandmarkPointContainer fixedLandmarks;
    TransformInitializerType::LandmarkPointContainer movingLandmarks;
    TransformInitializerType::LandmarkPointType      point;
    TransformInitializerType::LandmarkPointType      tmp;

    // Moving Landmarks = Fixed Landmarks rotated by 'angle' degrees and then
    // translated by the 'translation'. Offset can be used to move the fixed
    // landmarks around.
    const double nPI = 4.0 * std::atan(1.0);
    double       angle = 10 * nPI / 180.0;

    TransformInitializerType::LandmarkPointType translation;
    translation[0] = 6;
    translation[1] = 10;

    TransformInitializerType::LandmarkPointType offset;
    offset[0] = 10;
    offset[1] = 1;
    point[0] = 2 + offset[0];
    point[1] = 2 + offset[1];
    fixedLandmarks.push_back(point);

    tmp = point;

    point[0] = std::cos(angle) * point[0] - std::sin(angle) * point[1] + translation[0];
    point[1] = std::sin(angle) * tmp[0] + std::cos(angle) * point[1] + translation[1];
    movingLandmarks.push_back(point);

    point[0] = 2 + offset[0];
    point[1] = -2 + offset[1];
    fixedLandmarks.push_back(point);

    tmp = point;

    point[0] = std::cos(angle) * point[0] - std::sin(angle) * point[1] + translation[0];
    point[1] = std::sin(angle) * tmp[0] + std::cos(angle) * point[1] + translation[1];
    movingLandmarks.push_back(point);

    point[0] = -2 + offset[0];
    point[1] = 2 + offset[1];
    fixedLandmarks.push_back(point);

    tmp = point;

    point[0] = std::cos(angle) * point[0] - std::sin(angle) * point[1] + translation[0];
    point[1] = std::sin(angle) * tmp[0] + std::cos(angle) * point[1] + translation[1];
    movingLandmarks.push_back(point);

    point[0] = -2 + offset[0];
    point[1] = -2 + offset[1];
    fixedLandmarks.push_back(point);

    tmp = point;

    point[0] = std::cos(angle) * point[0] - std::sin(angle) * point[1] + translation[0];
    point[1] = std::sin(angle) * tmp[0] + std::cos(angle) * point[1] + translation[1];
    movingLandmarks.push_back(point);

    // No landmarks are set, it should throw
    ITK_TRY_EXPECT_EXCEPTION(initializer->InitializeTransform());
    initializer->SetFixedLandmarks(fixedLandmarks);
    ITK_TRY_EXPECT_EXCEPTION(initializer->InitializeTransform()); // Moving landmarks missing
    initializer->SetMovingLandmarks(movingLandmarks);

    success &= ExecuteAndExamine<TransformInitializerType>(initializer, fixedLandmarks, movingLandmarks);
  }

  {
    constexpr unsigned int Dimension = 3;
    using ImageType = itk::Image<PixelType, Dimension>;
    ImageType::Pointer fixedImage = CreateTestImage<Dimension>();
    ImageType::Pointer movingImage = CreateTestImage<Dimension>();

    using TransformType = itk::AffineTransform<double, Dimension>;
    TransformType::Pointer transform = TransformType::New();
    using TransformInitializerType = itk::LandmarkBasedTransformInitializer<TransformType, ImageType, ImageType>;
    TransformInitializerType::Pointer initializer = TransformInitializerType::New();

    ITK_EXERCISE_BASIC_OBJECT_METHODS(initializer, LandmarkBasedTransformInitializer, Object);

    initializer->SetTransform(transform);

    // Test that an exception is thrown if there aren't enough points
    ITK_TRY_EXPECT_EXCEPTION(initializer->InitializeTransform());

    const unsigned int numLandmarks(8);
    double             fixedLandMarkInit[numLandmarks][3] = {
      { -1.33671, -279.739, 176.001 },
      { 28.0989, -346.692, 183.367 },
      { -1.36713, -257.43, 155.36 },
      { -33.0851, -347.026, 180.865 },
      { -0.16083, -268.529, 148.96 },
      { -0.103873, -251.31, 122.973 },
      { 200, 200, 200 },  // dummy
      { -300, 100, 1000 } // dummy
    };
    double movingLandmarkInit[numLandmarks][3] = {
      { -1.65605 + 0.011, -30.0661, 20.1656 },
      { 28.1409, -93.1172 + 0.015, -5.34366 },
      { -1.55885, -0.499696 - 0.04, 12.7584 },
      { -33.0151 + 0.001, -92.0973, -8.66965 },
      { -0.189769, -7.3485, 1.74263 + 0.008 },
      { 0.1021, 20.2155, -12.8526 - 0.006 },
      { 200, 200, 200 },  // dummy
      { -300, 100, 1000 } // dummy

    };
    double weights[numLandmarks] = { 10, 1, 10, 1, 1, 1, 0.001, 0.001 };

    { // First Test with working Landmarks
      // These landmark should match properly
      constexpr unsigned int                           numWorkingLandmark = 6;
      TransformInitializerType::LandmarkPointContainer fixedLandmarks;
      TransformInitializerType::LandmarkPointContainer movingLandmarks;
      TransformInitializerType::LandmarkWeightType     landmarkWeights;

      for (unsigned i = 0; i < numWorkingLandmark; ++i)
      {
        TransformInitializerType::LandmarkPointType fixedPoint, movingPoint;

        for (unsigned j = 0; j < 3; ++j)
        {
          fixedPoint[j] = fixedLandMarkInit[i][j];
          movingPoint[j] = movingLandmarkInit[i][j];
        }
        fixedLandmarks.push_back(fixedPoint);
        movingLandmarks.push_back(movingPoint);
        landmarkWeights.push_back(weights[i]);
      }

      initializer->SetFixedLandmarks(fixedLandmarks);
      initializer->SetMovingLandmarks(movingLandmarks);
      initializer->SetLandmarkWeight(landmarkWeights);
      std::cerr << "Transform " << transform << std::endl;

      success &= ExecuteAndExamine<TransformInitializerType>(initializer, fixedLandmarks, movingLandmarks);
    }

    { // Test with dummy points
      // dummy points should not matched based on given weights
      constexpr unsigned int                           numDummyLandmark = 8;
      TransformInitializerType::LandmarkPointContainer fixedLandmarks;
      TransformInitializerType::LandmarkPointContainer movingLandmarks;
      TransformInitializerType::LandmarkWeightType     landmarkWeights;

      for (unsigned i = 0; i < numDummyLandmark; ++i)
      {
        TransformInitializerType::LandmarkPointType fixedPoint, movingPoint;

        for (unsigned j = 0; j < 3; ++j)
        {
          fixedPoint[j] = fixedLandMarkInit[i][j];
          movingPoint[j] = movingLandmarkInit[i][j];
        }
        fixedLandmarks.push_back(fixedPoint);
        movingLandmarks.push_back(movingPoint);
        landmarkWeights.push_back(weights[i]);
      }

      initializer->SetFixedLandmarks(fixedLandmarks);
      initializer->SetMovingLandmarks(movingLandmarks);
      initializer->SetLandmarkWeight(landmarkWeights);
      std::cerr << "Transform " << transform << std::endl;

      success &= ExecuteAndExamine<TransformInitializerType>(initializer, fixedLandmarks, movingLandmarks, 2);
    } // Second test with dummy
  }

  {
    std::cout << "\nTesting Landmark alignment with BSplineTransform..." << std::endl;
    constexpr unsigned int Dimension = 3;
    using FixedImageType = itk::Image<PixelType, Dimension>;
    using MovingImageType = itk::Image<PixelType, Dimension>;

    FixedImageType::Pointer  fixedImage = CreateTestImage<Dimension>();
    MovingImageType::Pointer movingImage = CreateTestImage<Dimension>();

    FixedImageType::PointType origin;
    origin[0] = -5;
    origin[1] = -5;
    origin[2] = -5;
    fixedImage->SetOrigin(origin);

    // Set the transform type
    constexpr unsigned int SplineOrder = 3;
    using TransformType = itk::BSplineTransform<double, FixedImageType::ImageDimension, SplineOrder>;
    TransformType::Pointer transform = TransformType::New();

    using TransformInitializerType =
      itk::LandmarkBasedTransformInitializer<TransformType, FixedImageType, MovingImageType>;
    TransformInitializerType::Pointer initializer = TransformInitializerType::New();

    ITK_EXERCISE_BASIC_OBJECT_METHODS(initializer, LandmarkBasedTransformInitializer, Object);

    TransformInitializerType::LandmarkPointContainer fixedLandmarks;
    TransformInitializerType::LandmarkPointContainer movingLandmarks;
    Init3DPoints<TransformInitializerType>(fixedLandmarks, movingLandmarks);

    constexpr unsigned int numLandmarks = 4;
    double                 weights[numLandmarks] = { 1, 3, 0.01, 0.5 };

    TransformInitializerType::LandmarkWeightType landmarkWeights;
    for (double weight : weights)
    {
      landmarkWeights.push_back(weight);
    }

    initializer->SetFixedLandmarks(fixedLandmarks);
    initializer->SetMovingLandmarks(movingLandmarks);
    initializer->SetLandmarkWeight(landmarkWeights);
    initializer->SetTransform(transform);
    initializer->SetBSplineNumberOfControlPoints(8);

    // Test that an exception is thrown if the reference image isn't set
    ITK_TRY_EXPECT_EXCEPTION(initializer->InitializeTransform());

    // Now set the reference image and initialization should work
    initializer->SetReferenceImage(fixedImage);

    success &= ExecuteAndExamine<TransformInitializerType>(initializer, fixedLandmarks, movingLandmarks);
  }

  {
    using TransformType = itk::Transform<float, 3, 3>;

    using TransformInitializerType = itk::LandmarkBasedTransformInitializer<TransformType>;

    TransformInitializerType::Pointer initializer = TransformInitializerType::New();

    ITK_EXERCISE_BASIC_OBJECT_METHODS(initializer, LandmarkBasedTransformInitializer, Object);
  }

  if (!success)
  {
    return EXIT_FAILURE;
  }
  std::cout << "Test PASSED!" << std::endl;
  return EXIT_SUCCESS;
}
