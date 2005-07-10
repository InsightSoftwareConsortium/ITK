/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkArrowSpatialObjectTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif

/**
 * This is a test file for the itkArrowSpatialObject class.
 */

#include "itkArrowSpatialObject.h"

int itkArrowSpatialObjectTest(int, char* [])
{
  typedef itk::ArrowSpatialObject<3>   ArrowType;
  
  ArrowType::Pointer myArrow = ArrowType::New(); 

  // Testing the length
  std::cout << "Testing length : ";
  myArrow->SetLength(2);
  if(myArrow->GetLength() != 2)
    {
    std::cout << "[FAILURE]" << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "[PASSED]" << std::endl;


  // Testing the direction of the arrow
  std::cout << "Testing direction : ";

  ArrowType::VectorType direction;
  direction.Fill(0);
  direction[1] = 1.0;

  myArrow->SetDirection(direction);
  if(myArrow->GetDirection()[0] != 0
    || myArrow->GetDirection()[1] != 1
    || myArrow->GetDirection()[2] != 0
    )
    {
    std::cout << "[FAILURE]" << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "[PASSED]" << std::endl;

  // Point consistency
  std::cout << "Is Inside: ";
  itk::Point<double,3> in;
  in[0]=0;in[1]=1;in[2]=0;
  itk::Point<double,3> out;
  out[0]=0;out[1]=2.1;out[2]=0;

  if(!myArrow->IsInside(in))
  {
    std::cout<<"[FAILED]"<<std::endl;
    return EXIT_FAILURE;
  }

  if(myArrow->IsInside(out))
  {
    std::cout<<"[FAILED]"<<std::endl;
    return EXIT_FAILURE;
  }

  std::cout<<"[PASSED]"<<std::endl;

 
  std::cout << "ComputeBoundingBox: ";
  myArrow->ComputeBoundingBox();
  ArrowType::BoundingBoxType * boundingBox = myArrow->GetBoundingBox();

  if( (boundingBox->GetBounds()[2] != 0 )
     || (boundingBox->GetBounds()[3] != 1 )
      )
    {
      std::cout<<"[FAILED]"<<std::endl;
      return EXIT_FAILURE;
    }

  std::cout << "[PASSED]" << std::endl;

  std::cout << "Test: [DONE]" << std::endl;
  return EXIT_SUCCESS;

}
