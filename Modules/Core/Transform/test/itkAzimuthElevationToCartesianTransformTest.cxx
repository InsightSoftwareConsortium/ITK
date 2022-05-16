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

#include "itkAzimuthElevationToCartesianTransform.h"
#include "itkTestingMacros.h"

template <typename TPoint>
void
PrintPoint(const TPoint & p)
{
  for (unsigned int i = 0; i < TPoint::PointDimension; ++i)
  {
    std::cout << p[i] << ", ";
  }
  std::cout << std::endl;
}

int
itkAzimuthElevationToCartesianTransformTest(int argc, char * argv[])
{
  if (argc != 7)
  {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv)
              << " maxAzimuth maxElevation radiusSampleSize azimuthAngularSeparation elevationAngularSeparation "
                 "firstSampleDistance"
              << std::endl;
    return EXIT_FAILURE;
  }

  using CoordinateRepresentationType = double;
  using PointType = itk::Point<CoordinateRepresentationType, 3>;

  const CoordinateRepresentationType ACCEPTABLE_ERROR = 1E-10;

  using AzimuthElevationToCartesianTransformType =
    itk::AzimuthElevationToCartesianTransform<CoordinateRepresentationType>;

  auto transform = AzimuthElevationToCartesianTransformType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(transform, AzimuthElevationToCartesianTransform, AffineTransform);


  auto maxAzimuth = static_cast<long>(std::stoi(argv[1]));
  transform->SetMaxAzimuth(maxAzimuth);
  ITK_TEST_SET_GET_VALUE(maxAzimuth, transform->GetMaxAzimuth());

  auto maxElevation = static_cast<long>(std::stoi(argv[2]));
  transform->SetMaxElevation(maxElevation);
  ITK_TEST_SET_GET_VALUE(maxElevation, transform->GetMaxElevation());

  auto radiusSampleSize = std::stod(argv[3]);
  transform->SetRadiusSampleSize(radiusSampleSize);
  ITK_TEST_SET_GET_VALUE(radiusSampleSize, transform->GetRadiusSampleSize());

  auto azimuthAngularSeparation = std::stod(argv[4]);
  transform->SetAzimuthAngularSeparation(azimuthAngularSeparation);
  ITK_TEST_SET_GET_VALUE(azimuthAngularSeparation, transform->GetAzimuthAngularSeparation());

  auto elevationAngularSeparation = std::stod(argv[5]);
  transform->SetFirstSampleDistance(elevationAngularSeparation);
  ITK_TEST_SET_GET_VALUE(elevationAngularSeparation, transform->GetElevationAngularSeparation());

  auto firstSampleDistance = std::stod(argv[6]);
  transform->SetFirstSampleDistance(firstSampleDistance);
  ITK_TEST_SET_GET_VALUE(firstSampleDistance, transform->GetFirstSampleDistance());

  transform->SetAzimuthElevationToCartesianParameters(1.0, 5.0, 45, 45);

  // test a bunch of points in all quadrants and those that could create exceptions
  PointType              q;
  std::vector<PointType> p;

  q[0] = 1;
  q[1] = 1;
  q[2] = 1;
  p.push_back(q);

  q[0] = 1;
  q[1] = 1;
  q[2] = -1;
  p.push_back(q);

  q[0] = 1;
  q[1] = -1;
  q[2] = 1;
  p.push_back(q);

  q[0] = 1;
  q[1] = -1;
  q[2] = -1;
  p.push_back(q);

  q[0] = -1;
  q[1] = 1;
  q[2] = 1;
  p.push_back(q);

  q[0] = -1;
  q[1] = 1;
  q[2] = -1;
  p.push_back(q);

  q[0] = -1;
  q[1] = -1;
  q[2] = 1;
  p.push_back(q);

  q[0] = -1;
  q[1] = -1;
  q[2] = -1;
  p.push_back(q);

  q[0] = -1;
  q[1] = 1;
  q[2] = 0;
  p.push_back(q);

  q[0] = 0;
  q[1] = 1;
  q[2] = 0;
  p.push_back(q);

  std::cout << "\n\n\t\t\tTransform Info:\n\n";
  transform->Print(std::cout);
  std::cout << "\n\n--------\n\n";

  for (auto & j : p)
  {
    std::cout << "original values of (theta,phi,r) p = " << std::endl;
    PrintPoint<PointType>(j);

    transform->SetForwardAzimuthElevationToCartesian();

    PointType answer = transform->TransformPoint(j);
    PrintPoint<PointType>(answer);

    PointType answerBackwards = transform->BackTransformPoint(answer);
    PrintPoint<PointType>(answerBackwards);

    transform->SetForwardCartesianToAzimuthElevation();
    PointType reverseDirectionAnswer = transform->BackTransformPoint(answerBackwards);
    PrintPoint<PointType>(reverseDirectionAnswer);

    PointType reverseDirectionAnswerBackwards = transform->TransformPoint(reverseDirectionAnswer);
    PrintPoint<PointType>(reverseDirectionAnswerBackwards);

    std::cout << "\n\n--------\n\n";

    bool same = true;
    for (unsigned int i = 0; i < PointType::PointDimension && same; ++i)
    {
      same = ((itk::Math::abs(j[i] - answerBackwards[i]) < ACCEPTABLE_ERROR) &&
              (itk::Math::abs(j[i] - reverseDirectionAnswerBackwards[i]) < ACCEPTABLE_ERROR) &&
              (itk::Math::abs(answer[i] - reverseDirectionAnswer[i]) < ACCEPTABLE_ERROR));
    }
    if (!same)
    {
      std::cout << "itkAzimuthElevationToCartesianTransformTest failed" << std::endl;
      return EXIT_FAILURE;
    }
  }

  // Check if itkAzimuthElevationToCartesianTransform returns the correct
  // TransformCategory.
  if (transform->GetTransformCategory() !=
      AzimuthElevationToCartesianTransformType::TransformCategoryEnum::UnknownTransformCategory)
  {
    std::cout << "itkAzimuthElevationToCartesianTransformTest failed" << std::endl;
    return EXIT_FAILURE;
  }

  std::cout << "itkAzimuthElevationToCartesianTransformTest passed" << std::endl;
  return EXIT_SUCCESS;
}
