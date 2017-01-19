/*=========================================================================
 *
 *  Copyright Insight Software Consortium
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

#include "itkAzimuthElevationToCartesianTransform.h"

template< typename TPoint >
void PrintPoint( const TPoint & p )
{
  for( unsigned int i=0; i < TPoint::PointDimension; ++i)
    {
    std::cout << p[i] << ", ";
    }
  std::cout << std::endl;
}

int itkAzimuthElevationToCartesianTransformTest(int, char *[])
{
  typedef  double                                     CoordinateRepresentationType;
  typedef  itk::Point<CoordinateRepresentationType,3> PointType;

  const CoordinateRepresentationType ACCEPTABLE_ERROR = 1E-10;

  typedef itk::AzimuthElevationToCartesianTransform<
    CoordinateRepresentationType
    > AzimuthElevationToCartesianTransformType;

  AzimuthElevationToCartesianTransformType::Pointer transform =
    AzimuthElevationToCartesianTransformType::New();

  transform->SetAzimuthElevationToCartesianParameters(1.0,5.0,45,45);

  // test a bunch of points in all quadrants and those that could create exceptions
  PointType q;
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

  for (unsigned int j = 0; j < p.size(); ++j)
    {
    std::cout << "original values of (theta,phi,r) p = " << std::endl;
    PrintPoint< PointType >(p.at(j));

    transform->SetForwardAzimuthElevationToCartesian();

    PointType answer = transform->TransformPoint(p.at(j));
    PrintPoint< PointType >(answer);

    PointType answerBackwards = transform->BackTransformPoint(answer);
    PrintPoint< PointType >(answerBackwards);

    transform->SetForwardCartesianToAzimuthElevation();
    PointType reverseDirectionAnswer = transform->BackTransformPoint(answerBackwards);
    PrintPoint< PointType >(reverseDirectionAnswer);

    PointType reverseDirectionAnswerBackwards = transform->TransformPoint(reverseDirectionAnswer);
    PrintPoint< PointType >(reverseDirectionAnswerBackwards);

    std::cout << "\n\n--------\n\n";

    bool same = true;
    for (unsigned int i = 0; i < PointType::PointDimension && same; ++i)
      {
      same = ((itk::Math::abs(p.at(j)[i] - answerBackwards[i]) < ACCEPTABLE_ERROR) &&
          (itk::Math::abs(p.at(j)[i] - reverseDirectionAnswerBackwards[i]) < ACCEPTABLE_ERROR) &&
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
  if(transform->GetTransformCategory() != AzimuthElevationToCartesianTransformType::UnknownTransformCategory)
  {
    std::cout << "itkAzimuthElevationToCartesianTransformTest failed" << std::endl;
    return EXIT_FAILURE;
  }

  std::cout << "itkAzimuthElevationToCartesianTransformTest passed" <<std::endl;
  return EXIT_SUCCESS;
}
