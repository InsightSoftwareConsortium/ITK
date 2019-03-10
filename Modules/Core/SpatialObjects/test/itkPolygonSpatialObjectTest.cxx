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

#include "itkPolygonSpatialObject.h"
#include <iostream>

int itkPolygonSpatialObjectTest(int, char *[])
{
  bool failed = false;
  using PolygonType = itk::PolygonSpatialObject<3>;

  //
  // create rectangle
  PolygonType::Pointer rectangle = PolygonType::New();
  rectangle->Print(std::cout);

  double d1[3] = {0.0, 0.0, 0.0};
  PolygonType::PointType p1(d1);
  double d2[3] = {2.0, 0.0, 0.0};
  PolygonType::PointType p2(d2);
  double d3[3] = {2.0, 1.0, 0.0};
  PolygonType::PointType p3(d3);
  double d4[3] = {0.0, 1.0, 0.0};
  PolygonType::PointType p4(d4);

  PolygonType::PolygonPointListType pList;
  PolygonType::PolygonPointType pPoint;
  pList.clear();
  pPoint.SetPositionInObjectSpace( p1 );
  pList.push_back( pPoint );
  pPoint.SetPositionInObjectSpace( p2 );
  pList.push_back( pPoint );
  pPoint.SetPositionInObjectSpace( p3 );
  pList.push_back( pPoint );
  pPoint.SetPositionInObjectSpace( p4 );
  pList.push_back( pPoint );
  rectangle->SetPoints(pList);
  rectangle->SetThicknessInObjectSpace(10);

  rectangle->Print(std::cout);
  //
  // test number of points
  std::cout << "Testing number of points for rectangle: ";
  if (rectangle->GetNumberOfPoints() != 4)
    {
    std::cout << "[Failed]" << std::endl;
    failed = true;
    }
  else
    {
    std::cout << "[Passed]" << std::endl;
    }

  //
  // test area
  std::cout << "Testing area for rectangle: ";
  if (rectangle->MeasureAreaInObjectSpace() != 2.0)
    {
    std::cout << "[Failed]" << std::endl;
    failed = true;
    }
  else
    {
    std::cout << "[Passed]" << std::endl;
    }

  //
  // test volume
  std::cout << "Testing volume for rectangle: ";
  if (rectangle->MeasureVolumeInObjectSpace() != 20.0)
    {
    std::cout << "[Failed]" << std::endl;
    failed = true;
    }
  else
    {
    std::cout << "[Passed]" << std::endl;
    }

  //
  // test perimeter
  std::cout << "Testing perimeter for rectangle: ";
  if (rectangle->MeasurePerimeterInObjectSpace() != 6.0)
    {
    std::cerr << "Wrong perimeter for rectangle" << std::endl;
    std::cout << "[Failed]" << std::endl;
    failed = true;
    }
  else
    {
    std::cout << "[Passed]" << std::endl;
    }

  //
  // test number of points
  std::cout << "Testing closest point for rectangle: ";
  double tp1[3] = {0.25, 0.0, 0.0};
  PolygonType::PointType testPoint1(tp1);
  const PolygonType::PolygonPointType closestPoint
    = rectangle->ClosestPointInWorldSpace(testPoint1);
  if (closestPoint.GetPositionInObjectSpace() != p1)
    {
    std::cout << "[Failed]" << std::endl;
    failed = true;
    }
  else
    {
    std::cout << "[Passed]" << std::endl;
    }

  //
  // test number of points
  std::cout << "Testing closest point for rectangle (2): ";
  double tp2[3] = {0.25, 5.0, 5.0};
  PolygonType::PointType testPoint2(tp2);
  const PolygonType::PolygonPointType closestPoint2
    = rectangle->ClosestPointInWorldSpace(testPoint2);
  if (closestPoint2.GetPositionInObjectSpace() != p4)
    {
    std::cout << "[Failed]" << std::endl;
    failed = true;
    }
  else
    {
    std::cout << "[Passed]" << std::endl;
    }

  return failed ? EXIT_FAILURE : EXIT_SUCCESS;
}
