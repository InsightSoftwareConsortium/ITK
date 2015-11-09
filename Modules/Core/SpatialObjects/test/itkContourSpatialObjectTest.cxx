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
#include "itkContourSpatialObject.h"
#include <iostream>
#include "itkMath.h"

/**
 * This is a test for itkContourSpatialObject.  It runs all methods and checks
 * the correctness of each result.  There are several potential issues
 * surrounding the IsInside method.  The IsEvaluable and ValueAt methods both
 * depend on IsInside and at the moment IsInside will always return false.  It
 * is unclear whether this is done intentionally to indicate that nothing can
 * be inside the contour or whether this is a bug that needs fixing.
 */
int itkContourSpatialObjectTest(int, char* [])
{

  //
  // Set up data
  //
  const unsigned int NumDimensions = 2;
  typedef itk::ContourSpatialObject<NumDimensions> SpatialObjectType;

  // contour is a unit square
  SpatialObjectType::ControlPointType pt1;
  pt1.SetPickedPoint(0,0);
  SpatialObjectType::ControlPointType  pt2;
  pt2.SetPickedPoint(1,0);
  SpatialObjectType::ControlPointType  pt3;
  pt3.SetPickedPoint(1,1);
  SpatialObjectType::ControlPointType  pt4;
  pt4.SetPickedPoint(0,1);

  SpatialObjectType::Pointer contour = SpatialObjectType::New();


  //
  // Test ComputeBoundingBox before data added
  //
  if (contour->ComputeLocalBoundingBox())
    {
    std::cout << "[FAILED] computed bounding box without data " << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "[PASSED] ComputeLocalBoundingBox before data" << std::endl;


  //
  // Test Control Points (SetControlPoints, GetControlPoints, GetNumberOfControlPoints,
  // GetControlPoint)
  //
  SpatialObjectType::ControlPointListType controlPointList;
  controlPointList.push_back(pt1);
  controlPointList.push_back(pt2);
  controlPointList.push_back(pt3);
  controlPointList.push_back(pt4);

  contour->SetControlPoints(controlPointList);

  // check number of points
  if (contour->GetNumberOfControlPoints() != 4)
    {
    std::cout << "[FAILED] Did not add the right number of control points" << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "[PASSED] GetNumberOfControlPoints" << std::endl;

  // check values of points
  if (itk::Math::NotAlmostEquals( contour->GetControlPoints()[0].GetPickedPoint()[0], pt1.GetPickedPoint()[0] ) ||
      itk::Math::NotAlmostEquals( contour->GetControlPoints()[0].GetPickedPoint()[1], pt1.GetPickedPoint()[1] ) ||
      itk::Math::NotAlmostEquals( contour->GetControlPoints()[1].GetPickedPoint()[0], pt2.GetPickedPoint()[0] ) ||
      itk::Math::NotAlmostEquals( contour->GetControlPoints()[1].GetPickedPoint()[1], pt2.GetPickedPoint()[1] ) ||
      itk::Math::NotAlmostEquals( contour->GetControlPoints()[2].GetPickedPoint()[0], pt3.GetPickedPoint()[0] ) ||
      itk::Math::NotAlmostEquals( contour->GetControlPoints()[2].GetPickedPoint()[1], pt3.GetPickedPoint()[1] ) ||
      itk::Math::NotAlmostEquals( contour->GetControlPoints()[3].GetPickedPoint()[0], pt4.GetPickedPoint()[0] ) ||
      itk::Math::NotAlmostEquals( contour->GetControlPoints()[3].GetPickedPoint()[1], pt4.GetPickedPoint()[1] ))
    {
    std::cout << "[FAILED] Did not add/retrieve control point list correctly" << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "[PASSED] Set/GetControlPoints" << std::endl;

  // check retrieval of a single point
  if (itk::Math::NotAlmostEquals(contour->GetControlPoint(0)->GetPickedPoint()[0], pt1.GetPickedPoint()[0]) ||
      itk::Math::NotAlmostEquals(contour->GetControlPoint(0)->GetPickedPoint()[1], pt1.GetPickedPoint()[1]))
    {
    std::cout << "[FAILED] Did not retrieve single control point correctly" << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "[PASSED] GetControlPoint" << std::endl;


  //
  // Test Set/Get Closed
  //

  // first set to not closed and test
  contour->SetClosed(false);
  if (contour->GetClosed())
    {
    std::cout << "[FAILED] Did not set/retrieve closed property correctly" << std::endl;
    return EXIT_FAILURE;
    }

  // then set it to closed and test
  contour->SetClosed(true);
  if (!contour->GetClosed())
    {
    std::cout << "[FAILED] Did not set/retrieve closed property correctly" << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "[PASSED] Set/GetClosed" << std::endl;


  //
  // Test Set/Get DisplayOrientation
  //
  contour->SetDisplayOrientation(1);
  if (contour->GetDisplayOrientation() != 1)
    {
    std::cout << "[FAILED] Did not set/retrieve display orientation correctly" << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "[PASSED] Set/GetDisplayOrientation" << std::endl;


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
  contour->SetInterpolationType(SpatialObjectType::LINEAR_INTERPOLATION);
  if (contour->GetInterpolationType() != SpatialObjectType::LINEAR_INTERPOLATION)
    {
    std::cout << "[FAILED] Did not set/retrieve interpolation type correctly" << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "[PASSED] Set/GetInterpolationType" << std::endl;


  //
  // Test Interpolation Points (SetInterpolationPoints, GetInterpolationPoints,
  // GetNumberOfInterpolationPoints, GetInterpolationPoint)
  //
  SpatialObjectType::InterpolatedPointType intPt1;
  intPt1.SetPosition(0,0.5);
  SpatialObjectType::InterpolatedPointType  intPt2;
  intPt2.SetPosition(0.5,0);

  SpatialObjectType::InterpolatedPointListType interpPointList;
  interpPointList.push_back(intPt1);
  interpPointList.push_back(intPt2);

  contour->SetInterpolatedPoints(interpPointList);

  // check number of points
  if (contour->GetNumberOfInterpolatedPoints() != 2)
    {
    std::cout << "[FAILED] Did not add the right number of interpolated points" << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "[PASSED] GetNumberOfInterpolatedPoints" << std::endl;

  // check values of points
  if (itk::Math::NotAlmostEquals(contour->GetInterpolatedPoints()[0].GetPosition()[0], intPt1.GetPosition()[0]) ||
      itk::Math::NotAlmostEquals(contour->GetInterpolatedPoints()[0].GetPosition()[1], intPt1.GetPosition()[1]) ||
      itk::Math::NotAlmostEquals(contour->GetInterpolatedPoints()[1].GetPosition()[0], intPt2.GetPosition()[0]) ||
      itk::Math::NotAlmostEquals(contour->GetInterpolatedPoints()[1].GetPosition()[1], intPt2.GetPosition()[1]))
    {
    std::cout << "[FAILED] Did not add/retrieve interpolated point list correctly" << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "[PASSED] Set/GetInterpolatedPoints" << std::endl;

  // check retrieval of a single point
  if (itk::Math::NotAlmostEquals(contour->GetInterpolatedPoint(0)->GetPosition()[0], intPt1.GetPosition()[0]) ||
      itk::Math::NotAlmostEquals(contour->GetInterpolatedPoint(0)->GetPosition()[1], intPt1.GetPosition()[1]))
    {
    std::cout << "[FAILED] Did not retrieve single interpolated point correctly" << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "[PASSED] GetInterpolatedPoint" << std::endl;


  //
  // Test ComputeLocalBoundingBox
  //
  if (!contour->ComputeLocalBoundingBox())
    {
    std::cout << "[FAILED] faild bounding box computation" << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "[PASSED] ComputeLocalBoundingBox" << std::endl;


  //
  // Test IsInside (at this point, this should always return false)
  //
  SpatialObjectType::PointType testPoint;
  testPoint[0] = 0;
  testPoint[1] = 0;
  if (contour->IsInside(testPoint))
    {
    std::cout << "[FAILED] Somehow returned true for IsInside" << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "[PASSED] IsInside" << std::endl;


  //
  // Test IsEvaluableAt (should always return false since IsInside always returns false)
  //
  if (contour->IsEvaluableAt(testPoint))
    {
    std::cout << "[FAILED] Somehow returned true for IsEvaluableAt" << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "[PASSED] IsEvaluableAt" << std::endl;


  //
  // Test ValueAt (should always return false and val=0 since IsInside always returns false)
  //
  double val = -1;
  double* valPtr = &val;
  if (contour->ValueAt(testPoint, *valPtr) || itk::Math::NotExactlyEquals(val, contour->GetDefaultOutsideValue()))
    {
    std::cout << "[FAILED] Somehow returned true for ValueAt" << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "[PASSED] ValueAt" << std::endl;


  //
  // Run PrintSelf for the sake of coverage (and to make sure no segfault/exceptions arise)
  //
  itk::Indent idt;
  contour->Print(std::cout, idt);


  //
  // All tests executed successfully
  //
  return EXIT_SUCCESS;

}
