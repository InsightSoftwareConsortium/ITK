/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkEllipseSpatialObjectTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

/**
 * This is a test file for the itkEllipseSpatialObject class.
 */

#include "itkEllipseSpatialObject.h"

int itkEllipseSpatialObjectTest(int, char** const)
{
  typedef itk::EllipseSpatialObject<4>   EllipseType;
  
  EllipseType::Pointer myEllipse = EllipseType::New();
  EllipseType::ArrayType radius;

  for(unsigned int i = 0; i<4 ;i++)
  {
    radius[i] = i;
  }
  
  std::cout << "Testing radii : ";
  
  myEllipse->SetRadius(radius);
  EllipseType::ArrayType radius2 = myEllipse->GetRadius();
  for(unsigned int i = 0; i<4 ;i++)
  {
    if(radius2[i]!=i)
    {
      std::cout << "[FAILURE]" << std::endl;
      return EXIT_FAILURE;
    }
  }
  std::cout << "[PASSED]" << std::endl;
  
  myEllipse->SetRadius(3);
 EllipseType::ArrayType radius3 = myEllipse->GetRadius();
  std::cout << "Testing Global radii : ";
  for(unsigned int i = 0; i<4 ;i++)
  {
    if(radius3[i]!=3)
    {
      std::cout << "[FAILURE]" << std::endl;
      return EXIT_FAILURE;
    }
  }

  std::cout << "[PASSED]" << std::endl;

   // Point consistency
  std::cout << "Is Inside: ";
  itk::Point<double,4> in;
  in[0]=1;in[1]=2;in[2]=1;in[3]=1;
  itk::Point<double,4> out;
  out[0]=0;out[1]=4;out[2]=0;out[3]=0;

  if(!myEllipse->IsInside(in))
  {
    std::cout<<"[FAILED]"<<std::endl;
    return EXIT_FAILURE;
  }

  if(myEllipse->IsInside(out))
  {
    std::cout<<"[FAILED]"<<std::endl;
    return EXIT_FAILURE;
  }

  std::cout<<"[PASSED]"<<std::endl;
  return EXIT_SUCCESS;

}
