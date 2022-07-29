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

#include "itkLandmarkSpatialObject.h"
#include "itkMath.h"
#include "itkTestingMacros.h"

int
itkLandmarkSpatialObjectTest(int, char *[])
{
  using LandmarkType = itk::LandmarkSpatialObject<3>;
  using LandmarkPointer = LandmarkType::Pointer;
  using LandmarkPointType = LandmarkType::LandmarkPointType;
  using PointType = LandmarkType::PointType;


  std::cout << "==================================" << std::endl;
  std::cout << "Testing LandmarkSpatialObject:" << std::endl << std::endl;

  LandmarkType::LandmarkPointListType list;

  unsigned int i;
  for (i = 0; i < 10; ++i)
  {
    LandmarkPointType p;
    PointType         pnt;

    pnt[0] = i;
    pnt[1] = i + 1;
    pnt[2] = i + 2;
    p.SetPositionInObjectSpace(pnt);
    p.SetBlue(i);
    p.SetGreen(i + 1);
    p.SetRed(i + 2);
    p.SetAlpha(i + 3);
    list.push_back(p);
  }

  // Create a Landmark Spatial Object
  LandmarkPointer landmark = LandmarkType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(landmark, LandmarkSpatialObject, PointBasedSpatialObject);

  landmark->GetProperty().SetName("Landmark 1");
  landmark->SetId(1);
  landmark->SetPoints(list);
  landmark->Update();

  // Number of points
  std::cout << "Testing Consistency: " << std::endl;
  std::cout << "Number of Points: ";

  landmark->Print(std::cout);

  if (landmark->GetPoints().size() != 10)
  {
    std::cout << "[FAILED]" << std::endl;
    return EXIT_FAILURE;
  }
  else
  {
    std::cout << "[PASSED]" << std::endl;
  }

  // Point consistency
  std::cout << "Point consistency: ";

  LandmarkType::LandmarkPointListType::const_iterator it = landmark->GetPoints().begin();

  i = 0;
  while (it != landmark->GetPoints().end())
  {
    for (unsigned int d = 0; d < 3; ++d)
    {
      if (itk::Math::NotExactlyEquals(it->GetPositionInObjectSpace()[d], i + d))
      {
        std::cout << "[FAILED]" << std::endl;
        return EXIT_FAILURE;
      }
    }
    it++;
    i++;
  }

  std::cout << "[PASSED]" << std::endl;

  // Point consistency
  std::cout << "Is Inside: ";
  itk::Point<double, 3> in;
  in[0] = 1;
  in[1] = 2;
  in[2] = 3;
  itk::Point<double, 3> out;
  out[0] = 0;
  out[1] = 0;
  out[2] = 0;

  if (!landmark->IsInsideInWorldSpace(in, LandmarkType::MaximumDepth, ""))
  {
    std::cout << "[FAILED]" << std::endl;
    return EXIT_FAILURE;
  }

  if (landmark->IsInsideInWorldSpace(out, LandmarkType::MaximumDepth, ""))
  {
    std::cout << "[FAILED]" << std::endl;
    return EXIT_FAILURE;
  }

  std::cout << "[PASSED]" << std::endl;

  std::cout << "Color: ";

  it = landmark->GetPoints().begin();

  i = 0;
  while (it != landmark->GetPoints().end())
  {
    for (unsigned int d = 0; d < 3; ++d)
    {
      if (itk::Math::NotExactlyEquals(it->GetBlue(), i))
      {
        std::cout << "[FAILED]" << std::endl;
        return EXIT_FAILURE;
      }
      if (itk::Math::NotExactlyEquals(it->GetGreen(), i + 1))
      {
        std::cout << "[FAILED]" << std::endl;
        return EXIT_FAILURE;
      }

      if (itk::Math::NotExactlyEquals(it->GetRed(), i + 2))
      {
        std::cout << "[FAILED]" << std::endl;
        return EXIT_FAILURE;
      }

      if (itk::Math::NotExactlyEquals(it->GetAlpha(), i + 3))
      {
        std::cout << "[FAILED]" << std::endl;
        return EXIT_FAILURE;
      }
    }
    it++;
    i++;
  }
  std::cout << "[PASSED]" << std::endl;

  // Testing IsEvaluableAt()
  std::cout << "Testing IsEvaluableAt() : ";
  if (!landmark->IsEvaluableAtInWorldSpace(in, LandmarkType::MaximumDepth, ""))
  {
    std::cout << "[FAILED]" << std::endl;
    return EXIT_FAILURE;
  }
  std::cout << "[PASSED]" << std::endl;

  // Testing ValueAt()
  std::cout << "Testing ValueAt() : ";
  double val = 0;
  if (!landmark->ValueAtInWorldSpace(in, val, LandmarkType::MaximumDepth, ""))
  {
    std::cout << "[FAILED]" << std::endl;
    return EXIT_FAILURE;
  }
  if (itk::Math::NotExactlyEquals(val, 1))
  {
    std::cout << "[FAILED]" << std::endl;
    return EXIT_FAILURE;
  }
  std::cout << "[PASSED]" << std::endl;


  std::cout << "Test finished" << std::endl;
  return EXIT_SUCCESS;
}
