/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkPlaneSpatialObjectTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

// Disable warning for long symbol names in this file only
#ifdef _MSC_VER
#pragma warning ( disable : 4786 )
#endif


/**
 * This is a test file for the itkPlaneSpatialObject class.
 */

#include "itkPlaneSpatialObject.h"

int itkPlaneSpatialObjectTest(int, char* [])
{
  typedef itk::PlaneSpatialObject<2>    PlaneType;
  typedef PlaneType::Pointer            PlanePointer;

  std::cout<<"=================================="<<std::endl;
  std::cout<<"Testing PlaneSpatialObject:"<<std::endl<<std::endl;


  // Create a Surface Spatial Object
  PlanePointer Plane = PlaneType::New();
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

  if(value != 1)
  {
     std::cout<<"[FAILED]"<<std::endl;
     return EXIT_FAILURE;
  }
  std::cout<<"[PASSED]"<<std::endl;



  return EXIT_SUCCESS;

}
