/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkTransformTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#include <iostream>

#include "itkTransform.h"
#include "itkMatrix.h"

   

int itkTransformTest(int, char* [] )
{



  typedef  itk::Transform<double,3,3>      TransformType;

  // itk::Transform cannot be instantiated because has
  // abstract methods.
    

  return EXIT_SUCCESS;

}
