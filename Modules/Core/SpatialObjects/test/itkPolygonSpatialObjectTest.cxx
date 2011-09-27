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
  typedef itk::PolygonSpatialObject<3>  PolygonType;

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
  rectangle->AddPoint(p1);
  rectangle->AddPoint(p2);
  rectangle->AddPoint(p3);
  rectangle->AddPoint(p4);
  rectangle->SetThickness(10);

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
  if (rectangle->MeasureArea() != 2.0)
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
  if (rectangle->MeasureVolume() != 20.0)
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
  if (rectangle->MeasurePerimeter() != 6.0)
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
  PolygonType::PointType closestPoint = rectangle->ClosestPoint(testPoint1);
  if (closestPoint != p1)
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
  closestPoint = rectangle->ClosestPoint(testPoint2);
  if (closestPoint != p4)
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
