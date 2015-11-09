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
// Disable warning for long symbol names in this file only

/**
 * This is a test file for the itkPlaneSpatialObject class.
 */

#include "itkPlaneSpatialObject.h"
#include "itkMath.h"

int itkPlaneSpatialObjectTest(int, char* [])
{
  typedef itk::PlaneSpatialObject<2>    PlaneType;
  typedef PlaneType::Pointer            PlanePointer;

  std::cout<<"=================================="<<std::endl;
  std::cout<<"Testing PlaneSpatialObject:"<<std::endl<<std::endl;


  // Create a Surface Spatial Object
  PlanePointer Plane = PlaneType::New();
  Plane->Print(std::cout);

  Plane->GetProperty()->SetName("Plane");
  Plane->SetId(1);
  Plane->GetId();

  PlaneType::PointType lowerPoint;
  lowerPoint[0]=-10;
  lowerPoint[1]=-10;

  PlaneType::PointType upperPoint;
  upperPoint[0]=10;
  upperPoint[1]=10;

  Plane->SetLowerPoint(lowerPoint);
  Plane->SetUpperPoint(upperPoint);

  Plane->GetLowerPoint();
  Plane->GetUpperPoint();

  Plane->ComputeBoundingBox();

  // Point consistency
  std::cout << "Is Inside: ";
  itk::Point<double,2> in;
  in[0]=0;in[1]=0;
  itk::Point<double,2> out;
  out[0]=11;out[1]=11;

  if(!Plane->IsInside(in))
  {
    std::cout<<"[FAILED]"<<std::endl;
    return EXIT_FAILURE;
  }

  if(Plane->IsInside(out))
  {
    std::cout<<"[FAILED]"<<std::endl;
    return EXIT_FAILURE;
  }

  std::cout<<"[PASSED]"<<std::endl;

  // Testing IsEvaluableAt()
  std::cout << "IsEvaluableAt: ";
  if(!Plane->IsEvaluableAt(in) || Plane->IsEvaluableAt(out))
  {
     std::cout<<"[FAILED]"<<std::endl;
     return EXIT_FAILURE;
  }
  std::cout<<"[PASSED]"<<std::endl;


  // Testing IsEvaluableAt()
  std::cout << "ValueAt: ";

  double value;
  if(!Plane->ValueAt(in,value))
  {
     std::cout<<"[FAILED]"<<std::endl;
     return EXIT_FAILURE;
  }

  if(itk::Math::NotExactlyEquals(value, 1))
  {
     std::cout<<"[FAILED]"<<std::endl;
     return EXIT_FAILURE;
  }

  Plane->Print(std::cout);

  std::cout<<"[PASSED]"<<std::endl;

  return EXIT_SUCCESS;

}
