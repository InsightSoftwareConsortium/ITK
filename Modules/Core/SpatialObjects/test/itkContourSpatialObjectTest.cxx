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
#include "itkContourSpatialObject.h"
#include <iostream>
#include "itkMath.h"
#include "itkTestingMacros.h"

/**
 * This is a test for itkContourSpatialObject.  It runs all methods and checks
 * the correctness of each result.  There are several potential issues
 * surrounding the IsInside method.  The IsEvaluable and ValueAt methods both
 * depend on IsInside and at the moment IsInside will always return false.  It
 * is unclear whether this is done intentionally to indicate that nothing can
 * be inside the contour or whether this is a bug that needs fixing.
 */
int
itkContourSpatialObjectTest(int, char *[])
{

  //
  // Set up data
  //
  constexpr unsigned int NumDimensions = 2;
  using SpatialObjectType = itk::ContourSpatialObject<NumDimensions>;

  // contour is a unit square
  SpatialObjectType::PointType pnt;

  SpatialObjectType::ControlPointType pt1;
  pnt[0] = 0;
  pnt[1] = 0;
  pt1.SetPickedPointInObjectSpace(pnt);

  SpatialObjectType::ControlPointType pt2;
  pnt[0] = 1;
  pnt[1] = 0;
  pt2.SetPickedPointInObjectSpace(pnt);

  SpatialObjectType::ControlPointType pt3;
  pnt[0] = 1;
  pnt[1] = 1;
  pt3.SetPickedPointInObjectSpace(pnt);

  SpatialObjectType::ControlPointType pt4;
  pnt[0] = 0;
  pnt[1] = 1;
  pt4.SetPickedPointInObjectSpace(pnt);

  auto contour = SpatialObjectType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(contour, ContourSpatialObject, PointBasedSpatialObject);


  auto interpolationFactor = 2u;
  contour->SetInterpolationFactor(interpolationFactor);
  ITK_TEST_SET_GET_VALUE(interpolationFactor, contour->GetInterpolationFactor());

  //
  // Test Control Points (SetControlPoints, GetControlPoints,
  // GetNumberOfControlPoints, GetControlPoint)
  //
  SpatialObjectType::ContourPointListType controlPointList;
  controlPointList.push_back(pt1);
  controlPointList.push_back(pt2);
  controlPointList.push_back(pt3);
  controlPointList.push_back(pt4);

  contour->SetControlPoints(controlPointList);

  //
  // Test Update() before data added
  //
  contour->Update();

  // check number of points
  if (contour->GetNumberOfControlPoints() != 4)
  {
    std::cout << "[FAILED] Did not add the right number of control points" << std::endl;
    return EXIT_FAILURE;
  }
  std::cout << "[PASSED] GetNumberOfControlPoints" << std::endl;

  // check values of points
  if (itk::Math::NotAlmostEquals(contour->GetControlPoints()[0].GetPickedPointInObjectSpace()[0],
                                 pt1.GetPickedPointInObjectSpace()[0]) ||
      itk::Math::NotAlmostEquals(contour->GetControlPoints()[0].GetPickedPointInObjectSpace()[1],
                                 pt1.GetPickedPointInObjectSpace()[1]) ||
      itk::Math::NotAlmostEquals(contour->GetControlPoints()[1].GetPickedPointInObjectSpace()[0],
                                 pt2.GetPickedPointInObjectSpace()[0]) ||
      itk::Math::NotAlmostEquals(contour->GetControlPoints()[1].GetPickedPointInObjectSpace()[1],
                                 pt2.GetPickedPointInObjectSpace()[1]) ||
      itk::Math::NotAlmostEquals(contour->GetControlPoints()[2].GetPickedPointInObjectSpace()[0],
                                 pt3.GetPickedPointInObjectSpace()[0]) ||
      itk::Math::NotAlmostEquals(contour->GetControlPoints()[2].GetPickedPointInObjectSpace()[1],
                                 pt3.GetPickedPointInObjectSpace()[1]) ||
      itk::Math::NotAlmostEquals(contour->GetControlPoints()[3].GetPickedPointInObjectSpace()[0],
                                 pt4.GetPickedPointInObjectSpace()[0]) ||
      itk::Math::NotAlmostEquals(contour->GetControlPoints()[3].GetPickedPointInObjectSpace()[1],
                                 pt4.GetPickedPointInObjectSpace()[1]))
  {
    std::cout << "[FAILED] Did not add/retrieve control point list correctly" << std::endl;
    return EXIT_FAILURE;
  }
  std::cout << "[PASSED] Set/GetControlPoints" << std::endl;

  // check retrieval of a single point
  if (itk::Math::NotAlmostEquals(contour->GetControlPoint(0)->GetPickedPointInObjectSpace()[0],
                                 pt1.GetPickedPointInObjectSpace()[0]) ||
      itk::Math::NotAlmostEquals(contour->GetControlPoint(0)->GetPickedPointInObjectSpace()[1],
                                 pt1.GetPickedPointInObjectSpace()[1]))
  {
    std::cout << "[FAILED] Did not retrieve single control point correctly" << std::endl;
    return EXIT_FAILURE;
  }
  std::cout << "[PASSED] GetControlPoint" << std::endl;


  //
  // Test Set/Get Closed
  //

  // first set to not closed and test
  auto isClosed = false;
  contour->SetIsClosed(isClosed);
  ITK_TEST_SET_GET_BOOLEAN(contour, IsClosed, isClosed);

  // then set it to closed and test
  isClosed = true;
  contour->SetIsClosed(true);
  ITK_TEST_SET_GET_BOOLEAN(contour, IsClosed, isClosed);


  //
  // Test Set/Get AttachedToSlice
  //

  // first test with no slice
  if (contour->GetAttachedToSlice() != -1)
  {
    std::cout << "[FAILED] Did not retrieve -1 when not slice" << std::endl;
    return EXIT_FAILURE;
  }

  // then test when attached to a slice
  contour->SetAttachedToSlice(1);
  if (contour->GetAttachedToSlice() != 1)
  {
    std::cout << "[FAILED] Did not set/retrieve proper slice" << std::endl;
    return EXIT_FAILURE;
  }
  std::cout << "[PASSED] Set/GetAttachedToSlice" << std::endl;


  //
  // Test Set/Get InterpolationType
  //
  contour->SetInterpolationMethod(SpatialObjectType::InterpolationMethodEnum::LINEAR_INTERPOLATION);
  if (contour->GetInterpolationMethod() != SpatialObjectType::InterpolationMethodEnum::LINEAR_INTERPOLATION)
  {
    std::cout << "[FAILED] Did not set/retrieve interpolation type correctly" << std::endl;
    return EXIT_FAILURE;
  }
  std::cout << "[PASSED] Set/GetInterpolationType" << std::endl;


  //
  // Test Interpolation Points (SetInterpolationPoints, GetInterpolationPoints,
  // GetNumberOfInterpolationPoints, GetInterpolationPoint)
  //
  SpatialObjectType::ContourPointType intPt1;
  pnt[0] = 0;
  pnt[1] = 0.5;
  intPt1.SetPositionInObjectSpace(pnt);
  SpatialObjectType::ContourPointType intPt2;
  pnt[0] = 0.5;
  pnt[1] = 0;
  intPt2.SetPositionInObjectSpace(pnt);

  SpatialObjectType::ContourPointListType interpPointList;
  interpPointList.push_back(intPt1);
  interpPointList.push_back(intPt2);

  contour->SetControlPoints(interpPointList);
  contour->SetInterpolationMethod(SpatialObjectType::InterpolationMethodEnum::NO_INTERPOLATION);
  contour->Update();

  // check number of points
  if (contour->GetNumberOfControlPoints() != 2)
  {
    std::cout << "[FAILED] Did not add the right number of interpolated points" << std::endl;
    return EXIT_FAILURE;
  }
  std::cout << "[PASSED] GetNumberOfInterpolatedPoints" << std::endl;

  // check values of points
  if (itk::Math::NotAlmostEquals(contour->GetPoints()[0].GetPositionInObjectSpace()[0],
                                 intPt1.GetPositionInObjectSpace()[0]) ||
      itk::Math::NotAlmostEquals(contour->GetPoints()[0].GetPositionInObjectSpace()[1],
                                 intPt1.GetPositionInObjectSpace()[1]) ||
      itk::Math::NotAlmostEquals(contour->GetPoints()[1].GetPositionInObjectSpace()[0],
                                 intPt2.GetPositionInObjectSpace()[0]) ||
      itk::Math::NotAlmostEquals(contour->GetPoints()[1].GetPositionInObjectSpace()[1],
                                 intPt2.GetPositionInObjectSpace()[1]))
  {
    std::cout << "[FAILED] Did not add/retrieve interpolated point list correctly" << std::endl;
    return EXIT_FAILURE;
  }
  std::cout << "[PASSED] Set/GetInterpolatedPoints" << std::endl;

  // check retrieval of a single point
  if (itk::Math::NotAlmostEquals(contour->GetPoint(0)->GetPositionInObjectSpace()[0],
                                 intPt1.GetPositionInObjectSpace()[0]) ||
      itk::Math::NotAlmostEquals(contour->GetPoint(0)->GetPositionInObjectSpace()[1],
                                 intPt1.GetPositionInObjectSpace()[1]))
  {
    std::cout << "[FAILED] Did not retrieve single interpolated point correctly" << std::endl;
    return EXIT_FAILURE;
  }
  std::cout << "[PASSED] GetInterpolatedPoint" << std::endl;


  //
  // Test Update()
  //
  try
  {
    contour->Update();
  }
  catch (const itk::ExceptionObject & excp)
  {
    std::cout << "[FAILED] failed Update()" << std::endl;
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
  }
  std::cout << "[PASSED] Update()" << std::endl;


  //
  // Test IsInside (at this point, this should always return false)
  //
  SpatialObjectType::PointType testPoint;
  testPoint[0] = 0;
  testPoint[1] = 0;
  if (contour->IsInsideInWorldSpace(testPoint))
  {
    std::cout << "[FAILED] Somehow returned true for IsInside" << std::endl;
    return EXIT_FAILURE;
  }
  std::cout << "[PASSED] IsInside" << std::endl;


  //
  // Test IsEvaluableAt (should always return false since IsInside
  // always returns false)
  //
  if (contour->IsEvaluableAtInWorldSpace(testPoint))
  {
    std::cout << "[FAILED] Somehow returned true for IsEvaluableAt" << std::endl;
    return EXIT_FAILURE;
  }
  std::cout << "[PASSED] IsEvaluableAt" << std::endl;


  //
  // Test ValueAt (should always return false and val=0 since IsInside
  //   always returns false)
  //
  double   val = -1;
  double * valPtr = &val;
  if (contour->ValueAtInWorldSpace(testPoint, *valPtr) ||
      itk::Math::NotExactlyEquals(val, contour->GetDefaultOutsideValue()))
  {
    std::cout << "[FAILED] Somehow returned true for ValueAt" << std::endl;
    return EXIT_FAILURE;
  }
  std::cout << "[PASSED] ValueAt" << std::endl;


  //
  // Run PrintSelf for the sake of coverage (and to make sure no
  // segfault/exceptions arise)
  //
  itk::Indent idt;
  contour->Print(std::cout, idt);

  // Test streaming enumeration for ContourSpatialObjectEnum::InterpolationMethod elements
  const std::set<itk::ContourSpatialObjectEnums::InterpolationMethod> allInterpolationMethod{
    itk::ContourSpatialObjectEnums::InterpolationMethod::NO_INTERPOLATION,
    itk::ContourSpatialObjectEnums::InterpolationMethod::EXPLICIT_INTERPOLATION,
    itk::ContourSpatialObjectEnums::InterpolationMethod::BEZIER_INTERPOLATION,
    itk::ContourSpatialObjectEnums::InterpolationMethod::LINEAR_INTERPOLATION
  };
  for (const auto & ee : allInterpolationMethod)
  {
    std::cout << "STREAMED ENUM VALUE ContourSpatialObjectEnums::InterpolationMethod: " << ee << std::endl;
  }


  std::cout << "Test finished" << std::endl;
  return EXIT_SUCCESS;
}
