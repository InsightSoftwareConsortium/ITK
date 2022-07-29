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

#include "itkTestingMacros.h"
#include "itkLineSpatialObject.h"
#include "itkMath.h"

int
itkLineSpatialObjectTest(int, char *[])
{
  using LineType = itk::LineSpatialObject<3>;
  using LinePointer = LineType::Pointer;
  using LinePointType = LineType::LinePointType;
  using VectorType = itk::CovariantVector<double, 3>;

  std::cout << "==================================" << std::endl;
  std::cout << "Testing LineSpatialObject:" << std::endl << std::endl;

  LineType::LinePointListType list;
  unsigned int                i;
  for (i = 0; i < 10; ++i)
  {
    LinePointType p;
    p.SetPositionInObjectSpace(i, i + 1, i + 2);
    VectorType normal1;
    VectorType normal2;
    for (unsigned int j = 0; j < 3; ++j)
    {
      normal1[j] = j;
      normal2[j] = j * 2;
    }

    p.SetNormalInObjectSpace(normal1, 0);
    p.SetNormalInObjectSpace(normal2, 1);
    list.push_back(p);
  }

  // For coverage
  LinePointType p;
  p.SetPositionInObjectSpace(0, 1, 2);
  p.Print(std::cout);

  // Create a Line Spatial Object
  LinePointer line = LineType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(line, LineSpatialObject, PointBasedSpatialObject);


  line->GetProperty().SetName("Line 1");
  line->SetId(1);
  line->SetPoints(list);
  line->Update();

  // Number of points
  std::cout << "Testing Consistency: " << std::endl;
  std::cout << "Number of Points: ";

  if (line->GetPoints().size() != 10)
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

  LineType::LinePointListType::const_iterator it = line->GetPoints().begin();

  i = 0;
  while (it != line->GetPoints().end())
  {
    for (unsigned int d = 0; d < 3; ++d)
    {
      if (itk::Math::NotExactlyEquals(it->GetPositionInWorldSpace()[d], i + d))
      {
        std::cout << "[FAILED]" << std::endl;
        return EXIT_FAILURE;
      }

      if (itk::Math::NotExactlyEquals((it->GetNormalInObjectSpace(0))[d], d))
      {
        std::cout << "[FAILED]" << std::endl;
        return EXIT_FAILURE;
      }

      if (itk::Math::NotExactlyEquals((it->GetNormalInObjectSpace(1))[d], 2 * d))
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

  if (!line->IsInsideInWorldSpace(in))
  {
    std::cout << "[FAILED]" << std::endl;
    return EXIT_FAILURE;
  }

  if (line->IsInsideInWorldSpace(out))
  {
    std::cout << "[FAILED]" << std::endl;
    return EXIT_FAILURE;
  }
  std::cout << "[PASSED]" << std::endl;

  // Testing IsEvaluableAt()
  std::cout << "IsEvaluableAt: ";
  if (!line->IsEvaluableAtInWorldSpace(in) || line->IsEvaluableAtInWorldSpace(out))
  {
    std::cout << "[FAILED]" << std::endl;
    return EXIT_FAILURE;
  }
  std::cout << "[PASSED]" << std::endl;


  // Testing IsEvaluableAt()
  std::cout << "ValueAt: ";

  double value;
  if (!line->ValueAtInWorldSpace(in, value))
  {
    std::cout << "[FAILED]" << std::endl;
    return EXIT_FAILURE;
  }

  if (itk::Math::NotExactlyEquals(value, 1))
  {
    std::cout << "[FAILED]" << std::endl;
    return EXIT_FAILURE;
  }
  std::cout << "[PASSED]" << std::endl;

  // Test Copy and Assignment for LinePointType
  {
    LinePointType pOriginal;

    // itk::SpatialObjectPoint
    pOriginal.SetId(250);
    pOriginal.SetColor(0.5, 0.4, 0.3, 0.2);
    pOriginal.SetPositionInObjectSpace(42, 41, 43);

    // itk::LineSpatialObjectPoint
    VectorType normal;
    normal.Fill(276);
    pOriginal.SetNormalInObjectSpace(normal, 0);

    // Copy
    LinePointType pCopy(pOriginal);
    // Assign
    LinePointType pAssign = pOriginal;

    std::vector<LinePointType> pointVector;
    pointVector.push_back(pCopy);
    pointVector.push_back(pAssign);

    for (const auto & pv : pointVector)
    {
      // itk::SpatialObjectPoint
      ITK_TEST_EXPECT_EQUAL(pOriginal.GetId(), pv.GetId());
      ITK_TEST_EXPECT_TRUE(itk::Math::AlmostEquals(pOriginal.GetRed(), pv.GetRed()));
      ITK_TEST_EXPECT_TRUE(itk::Math::AlmostEquals(pOriginal.GetGreen(), pv.GetGreen()));
      ITK_TEST_EXPECT_TRUE(itk::Math::AlmostEquals(pOriginal.GetBlue(), pv.GetBlue()));
      ITK_TEST_EXPECT_TRUE(itk::Math::AlmostEquals(pOriginal.GetAlpha(), pv.GetAlpha()));
      for (size_t j = 0; j < 3; ++j)
      {
        ITK_TEST_EXPECT_TRUE(
          itk::Math::AlmostEquals(pOriginal.GetPositionInObjectSpace()[j], pv.GetPositionInObjectSpace()[j]));
      }
      // itk::LineSpatialObjectPoint
      for (size_t j = 0; j < 3; ++j)
      {
        ITK_TEST_EXPECT_TRUE(
          itk::Math::AlmostEquals(pOriginal.GetNormalInObjectSpace(0)[j], pv.GetNormalInObjectSpace(0)[j]));
      }
    }
  }


  std::cout << "Test finished" << std::endl;
  return EXIT_SUCCESS;
}
